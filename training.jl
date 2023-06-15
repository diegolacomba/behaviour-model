using CSV, DataFrames

# Lectura de datos
data = CSV.read("data.txt", DataFrame)


# ---------------------------------------------------------------------------------
# ----------------------- METODOS PARA EL DEPURADO --------------------------------
# ---------------------------------------------------------------------------------

# Asociar habitaciones a sensores y actividades
function input_rooms()
    # Añadimos nueva columna al dataframe
    data[:,:habitacion] = missings(String15, nrow(data))
    # Asociamos sensor a habitacion
    data[in(["M028"]).(data.ID),:habitacion] .= "dormitorio"
    data[in(["M021"]).(data.ID),:habitacion] .= "dormitorio"
    data[in(["M020"]).(data.ID),:habitacion] .= "dormitorio"
    data[in(["M019"]).(data.ID),:habitacion] .= "dormitorio"
    data[in(["M025"]).(data.ID),:habitacion] .= "baño"
    data[in(["M013"]).(data.ID),:habitacion] .= "baño"
    data[in(["M017"]).(data.ID),:habitacion] .= "baño"
    data[in(["M018"]).(data.ID),:habitacion] .= "baño"
    data[in(["M023"]).(data.ID),:habitacion] .= "cocina"
    data[in(["M012"]).(data.ID),:habitacion] .= "cocina"
    data[in(["M014"]).(data.ID),:habitacion] .= "cocina"
    data[in(["M015"]).(data.ID),:habitacion] .= "cocina"
    data[in(["M016"]).(data.ID),:habitacion] .= "cocina"
    data[in(["M022"]).(data.ID),:habitacion] .= "cocina"
    data[in(["M026"]).(data.ID),:habitacion] .= "television"
    data[in(["M007"]).(data.ID),:habitacion] .= "television"
    data[in(["M008"]).(data.ID),:habitacion] .= "television"
    data[in(["M027"]).(data.ID),:habitacion] .= "salon"
    data[in(["M003"]).(data.ID),:habitacion] .= "salon"
    data[in(["M004"]).(data.ID),:habitacion] .= "salon"
    data[in(["M005"]).(data.ID),:habitacion] .= "salon"
    data[in(["M006"]).(data.ID),:habitacion] .= "salon"
    data[in(["M001"]).(data.ID),:habitacion] .= "salon"
    data[in(["M002"]).(data.ID),:habitacion] .= "salon"

    # Sustituimos "missing" por "" (necesario para la continuacion)
    data.description = replace(data.description, missing => "")
    data.activity = replace(data.activity, missing => "")

    # Asociamos actividad a habitacion
    data[in(["Bed_to_Toilet"]).(data.description),:habitacion] .= "baño"
    data[in(["Desk_Activity"]).(data.description),:habitacion] .= "television"
    data[in(["Dining_Rm_Activity"]).(data.description),:habitacion] .= "salon"
    data[in(["Eve_Meds"]).(data.description),:habitacion] .= "cocina"
    data[in(["Guest_Bathroom"]).(data.description),:habitacion] .= "baño"
    data[in(["Kitchen_Activity"]).(data.description),:habitacion] .= "cocina"
    data[in(["Leave_Home"]).(data.description),:habitacion] .= "salon"
    data[in(["Master_Bathroom"]).(data.description),:habitacion] .= "baño"
    data[in(["Watch_TV"]).(data.description),:habitacion] .= "television"
    data[in(["Sleep"]).(data.description),:habitacion] .= "dormitorio"
    data[in(["Read"]).(data.description),:habitacion] .= "salon"
    data[in(["Morning_Meds"]).(data.description),:habitacion] .= "cocina"
    data[in(["Master_Bedroom_Activity"]).(data.description),:habitacion] .= "dormitorio"

    # Eliminamos las observaciones sin asignación de habitación:
    dropmissing!(data,:habitacion,disallowmissing=true)
    # Eliminar observaciones por condición:
    filter!(row -> !(row.description == "" && row.OnOff == "OFF"), data)
end



# Asociar totalmente una actividad a una habitación
# Eliminar observaciones entre begin y end
function remove_ADLintermediate()
    activity = false

    # Marcamos la observaciones a eliminar
    for i in 1:nrow(data)
        if data[i,:activity] == "begin"
            activity = true
        elseif data[i,:activity] == "end"
            activity = false
        elseif activity
                data[i,:habitacion] = "remove"
        end
    end
    
    
    # Eliminar observaciones por condición:
    filter!(row -> !(row.habitacion == "remove"), data)   
end



# Eliminar observaciones individuales
# Es decir, si la habitacion es distinta de la observación anterior y siguiente
function remove_unpaired()
    # Primera observacion:
    if data[1,:habitacion] != data[2,:habitacion]
        data[1,:description] = "remove"
    end

    # Resto:
    prev = data[1,:habitacion]
    for i in 2:nrow(data)-1

        current = data[i,:habitacion]
        sig = data[i+1,:habitacion]

        # Si se diferencia de la anterior Y siguiente
        if (current != prev && current != sig)
            # Eliminamos intermedio
            data[i,:description] = "remove"
        end

        prev = current

    end

    # Eliminar observaciones por condición:
    filter!(row -> !(row.description == "remove"), data)    
end


# Eliminar observaciones intermedias (minimo tres seguidas)
# Resultado: parejas inicio-fin estancia
function remove_intermediate()
    prev = data[1,:habitacion]

    for i in 2:nrow(data)-1
        
        current = data[i,:habitacion]
        sig = data[i+1,:habitacion]

        # Si hay tres seguidas
        if (prev == current && prev == sig)
            # Eliminamos intermedio
            data[i,:habitacion] = "remove"
        else
            prev = current
        end

    end

    # Eliminar observaciones por condición:
    filter!(row -> !(row.habitacion == "remove"), data)
end


# Duración (minutos) entre 2 observaciones
function duration(_begin, _end)
    hour_begin = parse(Int,split(_begin,":")[1])
    hour_end = parse(Int,split(_end,":")[1])
    hours = hour_end - hour_begin
    
    if hours < 0
        hours+=24
    end
    
    minute_begin = parse(Int,split(_begin,":")[2])
    minute_end = parse(Int,split(_end,":")[2])
    minutes = minute_end - minute_begin
    
    if minutes < 0
        minutes+=60
        hours-=1
    end
    
    duration=(hours*60)+minutes
    return duration
end


# Eliminar estancias de menos de 2 minutos
function remove_short()
    # Eliminamos estancias de menos de 2 minutos
    for i in 1:2:nrow(data)
        minutes = duration(data[i,:HOUR],data[i+1,:HOUR])
        
        if minutes < 2
            data[i,:habitacion] = "remove"
            data[i+1,:habitacion] = "remove"
        end
    end

    # Eliminar observaciones por condición:
    filter!(row -> !(row.habitacion == "remove"), data)
end

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------
# ----------------------------------- DEPURADO ------------------------------------
# ---------------------------------------------------------------------------------

# Pasos depurado:
input_rooms()
remove_ADLintermediate()
remove_unpaired()
remove_intermediate()
remove_short()
remove_intermediate()
data2 = select(data,1,2,7)
CSV.write("data_depured.txt", data2)

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------
# ---------------- METODOS PARA GENERAR PROBABILIDADES DE INICIO ------------------
# ---------------------------------------------------------------------------------

#= data2 = select(data2, 1, :HOUR => ByRow(x -> get.(Ref(split(x, ":")), 1:2, missing)) => [:HORA, :MINUTO], 3)
data2[!,:MINUTO] = [parse(Int,x) for x in data2[:,:MINUTO]]

data_aux1 = filter(row -> ((row.HORA == "00" && row.MINUTO < 5) || (row.HORA == "23" && row.MINUTO > 55)), data2)
data_aux2 = filter(row -> ((row.HORA == "00" && row.MINUTO < 5) || (row.HORA == "23" && row.MINUTO > 55)), data2)
data_aux3 = filter(row -> ((row.HORA == "00" && row.MINUTO < 5) || (row.HORA == "23" && row.MINUTO > 55)), data2)
data_aux4 = filter(row -> ((row.HORA == "00" && row.MINUTO < 5) || (row.HORA == "23" && row.MINUTO > 55)), data2)
data_aux5 = filter(row -> ((row.HORA == "00" && row.MINUTO < 5) || (row.HORA == "23" && row.MINUTO > 55)), data2)
data_aux6 = filter(row -> ((row.HORA == "00" && row.MINUTO < 5) || (row.HORA == "23" && row.MINUTO > 55)), data2) =#

# Interesante trabajar con data_aux3 porque no se han eliminado observaciones intermedias

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# ---------------------- METODOS PARA GENERAR TRANSICIONES ------------------------
# ---------------------------------------------------------------------------------

# Vector con las observaciones correspondientes a la transición de interés
# Como máximo debe haber una diferencia de 5 minutos para considerarse transición
function transiciones(origin, ending)
    v = String15[]

    for i in 1:nrow(data)-1
        if data[i,:habitacion] == origin && data[i+1,:habitacion] == ending
            if duration(data[i,:HOUR],data[i+1,:HOUR]) <= 5
                push!(v, data[i,:HOUR])
            end
            # push!(v, data[i,:HOUR])
        end
    end

    return v
end


# Transformar hora:minutos a minutos
function hour2min(hours)
    minutes = Int[]
    for i in hours
        hour = parse(Int,split(i,":")[1])
        min = parse(Int,split(i,":")[2])
        
        res = (hour*60)+min
        # Separamos en intervalos de 5 minutos
        #res = ((hour*60)+min) ÷ 5 * 5

        push!(minutes,res)
    end
    return minutes
end


# Transformar minutos a radianes
function min2rad(minutes)
    radians = Float32[]
    for i in minutes
        rad = i / (24*60) * 2π
        push!(radians,rad)
    end
    return radians
end


# Transformar radianes a puntos en el circulo unitario
function rad2unitcircle(radians)
    x_axis = Float32[]
    y_axis = Float32[]
    for i in radians
        x = cos(i)
        push!(x_axis,x)
        y = sin(i)
        push!(y_axis,y)
    end
    return (x_axis, y_axis)
end


# ----------------------------- DORMITORIO -------------------------------------
function genera_transiciones_dormitorio()

    # bano
    dormitorio_bano_coords = rad2unitcircle(min2rad(hour2min(transiciones("dormitorio","baño"))))
    dormitorio_bano.x = dormitorio_bano_coords[1]
    dormitorio_bano.y = dormitorio_bano_coords[2]
    CSV.write("Transiciones\\dormitorio_bano.csv", dormitorio_bano)

    # COCINA
    dormitorio_cocina_coords = rad2unitcircle(min2rad(hour2min(transiciones("dormitorio","cocina"))))
    dormitorio_cocina.x = dormitorio_cocina_coords[1]
    dormitorio_cocina.y = dormitorio_cocina_coords[2]
    CSV.write("Transiciones\\dormitorio_cocina.csv", dormitorio_cocina)

    # TELEVISION
    dormitorio_tv_coords = rad2unitcircle(min2rad(hour2min(transiciones("dormitorio","television"))))
    dormitorio_tv.x = dormitorio_tv_coords[1]
    dormitorio_tv.y = dormitorio_tv_coords[2]
    CSV.write("Transiciones\\dormitorio_tv.csv", dormitorio_tv)

    # SALON
    dormitorio_salon_coords = rad2unitcircle(min2rad(hour2min(transiciones("dormitorio","salon"))))
    dormitorio_salon.x = dormitorio_salon_coords[1]
    dormitorio_salon.y = dormitorio_salon_coords[2]
    CSV.write("Transiciones\\dormitorio_salon.csv", dormitorio_salon)

end


# ------------------------------ BAÑO --------------------------------------
function genera_transiciones_bano()
    
    # DORMITORIO
    bano_dormitorio_coords = rad2unitcircle(min2rad(hour2min(transiciones("baño","dormitorio"))))
    bano_dormitorio.x = bano_dormitorio_coords[1]
    bano_dormitorio.y = bano_dormitorio_coords[2]
    CSV.write("Transiciones\\bano_dormitorio.csv", bano_dormitorio)

    # COCINA
    bano_cocina_coords = rad2unitcircle(min2rad(hour2min(transiciones("baño","cocina"))))
    bano_cocina.x = bano_cocina_coords[1]
    bano_cocina.y = bano_cocina_coords[2]
    CSV.write("Transiciones\\bano_cocina.csv", bano_cocina)

    # TELEVISION
    bano_tv_coords = rad2unitcircle(min2rad(hour2min(transiciones("baño","television"))))
    bano_tv.x = bano_tv_coords[1]
    bano_tv.y = bano_tv_coords[2]
    CSV.write("Transiciones\\bano_tv.csv", bano_tv)

    # SALON
    bano_salon_coords = rad2unitcircle(min2rad(hour2min(transiciones("baño","salon"))))
    bano_salon.x = bano_salon_coords[1]
    bano_salon.y = bano_salon_coords[2]
    CSV.write("Transiciones\\bano_salon.csv", bano_salon)

end


# ------------------------------ COCINA --------------------------------------
function genera_transiciones_cocina()
    
    # DORMITORIO
    cocina_dormitorio_coords = rad2unitcircle(min2rad(hour2min(transiciones("cocina","dormitorio"))))
    cocina_dormitorio.x = cocina_dormitorio_coords[1]
    cocina_dormitorio.y = cocina_dormitorio_coords[2]
    CSV.write("Transiciones\\cocina_dormitorio.csv", cocina_dormitorio)

    # bano
    cocina_bano_coords = rad2unitcircle(min2rad(hour2min(transiciones("cocina","baño"))))
    cocina_bano.x = cocina_bano_coords[1]
    cocina_bano.y = cocina_bano_coords[2]
    CSV.write("Transiciones\\cocina_bano.csv", cocina_bano)

    # TELEVISION
    cocina_tv_coords = rad2unitcircle(min2rad(hour2min(transiciones("cocina","television"))))
    cocina_tv.x = cocina_tv_coords[1]
    cocina_tv.y = cocina_tv_coords[2]
    CSV.write("Transiciones\\cocina_tv.csv", cocina_tv)

    # SALON
    cocina_salon_coords = rad2unitcircle(min2rad(hour2min(transiciones("cocina","salon"))))
    cocina_salon.x = cocina_salon_coords[1]
    cocina_salon.y = cocina_salon_coords[2]
    CSV.write("Transiciones\\cocina_salon.csv", cocina_salon)

end


# ------------------------------ TELEVISION --------------------------------------
function genera_transiciones_tv()
    
    # DORMITORIO
    tv_dormitorio_coords = rad2unitcircle(min2rad(hour2min(transiciones("television","dormitorio"))))
    tv_dormitorio.x = tv_dormitorio_coords[1]
    tv_dormitorio.y = tv_dormitorio_coords[2]
    CSV.write("Transiciones\\tv_dormitorio.csv", tv_dormitorio)


    # bano
    tv_bano_coords = rad2unitcircle(min2rad(hour2min(transiciones("television","baño"))))
    tv_bano.x = tv_bano_coords[1]
    tv_bano.y = tv_bano_coords[2]
    CSV.write("Transiciones\\tv_bano.csv", tv_bano)

    # COCINA
    tv_cocina_coords = rad2unitcircle(min2rad(hour2min(transiciones("television","cocina"))))
    tv_cocina.x = tv_cocina_coords[1]
    tv_cocina.y = tv_cocina_coords[2]
    CSV.write("Transiciones\\tv_cocina.csv", tv_cocina)

    # SALON
    tv_salon_coords = rad2unitcircle(min2rad(hour2min(transiciones("television","salon"))))
    tv_salon.x = tv_salon_coords[1]
    tv_salon.y = tv_salon_coords[2]
    CSV.write("Transiciones\\tv_salon.csv", tv_salon)

end


# ------------------------------ SALON --------------------------------------
function genera_transiciones_salon()
    
    # DORMITORIO
    salon_dormitorio_coords = rad2unitcircle(min2rad(hour2min(transiciones("salon","dormitorio"))))
    salon_dormitorio.x = salon_dormitorio_coords[1]
    salon_dormitorio.y = salon_dormitorio_coords[2]
    CSV.write("Transiciones\\salon_dormitorio.csv", salon_dormitorio)

    # bano
    salon_bano_coords = rad2unitcircle(min2rad(hour2min(transiciones("salon","baño"))))
    salon_bano.x = salon_bano_coords[1]
    salon_bano.y = salon_bano_coords[2]
    CSV.write("Transiciones\\salon_bano.csv", salon_bano)

    # COCINA
    salon_cocina_coords = rad2unitcircle(min2rad(hour2min(transiciones("salon","cocina"))))
    salon_cocina.x = salon_cocina_coords[1]
    salon_cocina.y = salon_cocina_coords[2]
    CSV.write("Transiciones\\salon_cocina.csv", salon_cocina)

    # TELEVISION
    salon_tv_coords = rad2unitcircle(min2rad(hour2min(transiciones("salon","television"))))
    salon_tv.x = salon_tv_coords[1]
    salon_tv.y = salon_tv_coords[2]
    CSV.write("Transiciones\\salon_tv.csv", salon_tv)

end

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------
# ----------------------------- GENERAR TRANSICIONES ------------------------------
# ---------------------------------------------------------------------------------

dormitorio_bano = DataFrame()
dormitorio_cocina = DataFrame()
dormitorio_tv = DataFrame()
dormitorio_salon = DataFrame()
bano_dormitorio = DataFrame()
bano_cocina = DataFrame()
bano_tv = DataFrame()
bano_salon = DataFrame()
cocina_dormitorio = DataFrame()
cocina_bano = DataFrame()
cocina_tv = DataFrame()
cocina_salon = DataFrame()
tv_dormitorio = DataFrame()
tv_bano = DataFrame()
tv_cocina = DataFrame()
tv_salon = DataFrame()
salon_dormitorio = DataFrame()
salon_bano = DataFrame()
salon_cocina = DataFrame()
salon_tv = DataFrame()

genera_transiciones_dormitorio()
genera_transiciones_bano()
genera_transiciones_cocina()
genera_transiciones_tv()
genera_transiciones_salon()

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
using RCall

# Leer transiciones
dormitorio_bano = CSV.read("Transiciones\\dormitorio_bano.csv",DataFrame)
dormitorio_cocina = CSV.read("Transiciones\\dormitorio_cocina.csv",DataFrame)
dormitorio_tv = CSV.read("Transiciones\\dormitorio_tv.csv",DataFrame)
dormitorio_salon = CSV.read("Transiciones\\dormitorio_salon.csv",DataFrame)

bano_dormitorio = CSV.read("Transiciones\\bano_dormitorio.csv",DataFrame)
bano_cocina = CSV.read("Transiciones\\bano_cocina.csv",DataFrame)
bano_tv = CSV.read("Transiciones\\bano_tv.csv",DataFrame)
bano_salon = CSV.read("Transiciones\\bano_salon.csv",DataFrame)

cocina_dormitorio = CSV.read("Transiciones\\cocina_dormitorio.csv",DataFrame)
cocina_bano = CSV.read("Transiciones\\cocina_bano.csv",DataFrame)
cocina_tv = CSV.read("Transiciones\\cocina_tv.csv",DataFrame)
cocina_salon = CSV.read("Transiciones\\cocina_salon.csv",DataFrame)

tv_dormitorio = CSV.read("Transiciones\\tv_dormitorio.csv",DataFrame)
tv_bano = CSV.read("Transiciones\\tv_bano.csv",DataFrame)
tv_cocina = CSV.read("Transiciones\\tv_cocina.csv",DataFrame)
tv_salon = CSV.read("Transiciones\\tv_salon.csv",DataFrame)

salon_dormitorio = CSV.read("Transiciones\\salon_dormitorio.csv",DataFrame)
salon_bano = CSV.read("Transiciones\\salon_bano.csv",DataFrame)
salon_cocina = CSV.read("Transiciones\\salon_cocina.csv",DataFrame)
salon_tv = CSV.read("Transiciones\\salon_tv.csv",DataFrame)

@rput dormitorio_bano
@rput dormitorio_cocina
@rput dormitorio_tv
@rput dormitorio_salon
@rput bano_dormitorio
@rput bano_cocina
@rput bano_tv
@rput bano_salon
@rput cocina_dormitorio
@rput cocina_bano
@rput cocina_tv
@rput cocina_salon
@rput tv_dormitorio
@rput tv_bano
@rput tv_cocina
@rput tv_salon
@rput salon_dormitorio
@rput salon_bano
@rput salon_cocina
@rput salon_tv

#Otra forma, ejecutar directamente un Script (ubicarse en espacio de trabajo previamente)
#R"source(\"prueba.R\")"
# R"""
# ...
#"""


# ---------------------------------------------------------------------------------
# -------------------------- ESTIMAR PARÁMETROS Von Mises--------------------------
# ---------------------------------------------------------------------------------
# ----------------------- MÉTODOS TEST KS de bondad de ajuste ---------------------
# ---------------------------------------------------------------------------------

# Transformar puntos en el circulo unitario a radianes
function coord2rad()
    R"""
    library(circular)

    dormitorio_bano_radians = coord2rad(dormitorio_bano)
    dormitorio_cocina_radians = coord2rad(dormitorio_cocina)
    dormitorio_salon_radians = coord2rad(dormitorio_salon)
    dormitorio_tv_radians = coord2rad(dormitorio_tv)

    bano_dormitorio_radians = coord2rad(bano_dormitorio)
    bano_cocina_radians = coord2rad(bano_cocina)
    bano_salon_radians = coord2rad(bano_salon)
    bano_tv_radians = coord2rad(bano_tv)

    cocina_dormitorio_radians = coord2rad(cocina_dormitorio)
    cocina_bano_radians = coord2rad(cocina_bano)
    cocina_salon_radians = coord2rad(cocina_salon)
    cocina_tv_radians = coord2rad(cocina_tv)

    salon_dormitorio_radians = coord2rad(salon_dormitorio)
    salon_bano_radians = coord2rad(salon_bano)
    salon_cocina_radians = coord2rad(salon_cocina)
    salon_tv_radians = coord2rad(salon_tv)

    tv_dormitorio_radians = coord2rad(tv_dormitorio)
    tv_bano_radians = coord2rad(tv_bano)
    tv_cocina_radians = coord2rad(tv_cocina)
    tv_salon_radians = coord2rad(tv_salon)
    """
    
end


# Obtener frecuencias acumuladas muestrales
function getFn(sample)
    n = size(sample)[1]
    tam = 1/n

    Fn = zeros(Float32,n);
    Fn[:,1] = sample
    for i in 1:n
        Fn[i] = tam*i
    end

    return Fn
end


# Leer los parámetros y descomponer theta en mu y kappa
function getParams(params)

    thetas=params[:theta]
    alpha=params[:alpha]

    mixtura = DataFrame(alpha = alpha, mu_x = 0.0, mu_y = 0.0, kappa = 0.0, theta_x = thetas[:,1], theta_y = thetas[:,2])

    for i in 1:size(mixtura)[1]

        kappa = norm(thetas[i,:])
        mu = thetas[i,:]/kappa

        mixtura[i,2:3] = mu
        mixtura[i,4] = kappa

    end

    return mixtura
end


# Obtener frecuencia acumulada bajo hipótesis nula (bajo parámetros estimados)
function getF0(sample,mixtura)

    alpha = mixtura[:,:alpha]
    kappa = mixtura[:,:kappa]

    @rput mixtura
    @rput kappa
    @rput sample


    R"""
    k = length(kappa)

    mu = numeric(k)
    for (i in 1:k) {
        mu[i] = coord2rad(mixtura[i,2:3])
    }

    mu = circular(mu, units="radians")
    muestra = circular(sample, units="radians")

    F0s = matrix(0,nrow=length(muestra),ncol=k)
    for (i in 1:k) {
        F0s[,i] = pvonmises(muestra, mu=mu[i], kappa=kappa[i], from=0)
    }
    """
    @rget F0s

    F0 = zeros(Float32,size(sample)[1]);
    for i in 1:size(alpha)[1]
        F0 = F0 + alpha[i]*F0s[:,i]
    end

    return F0
end


# Pasamos el test con 0.05
function runKS(Fn,F0)
    D = maximum(abs.(Fn-F0))

    if size(Fn)[1] > 40
        k = 1.36/sqrt(size(Fn)[1])
        
        # Si D esta FUERA de la zona de rechazo devolvemos TRUE como que se pasa el test.
        return D < k
    else
        # Si D < 40 deolver FALSE hasta que se pueda acceder a la tabla
        return false
    end
end


# Obtención de EMV calculable y KSTest
function testParams(coords,radians)

    # Estimación de parámetros:
    #   1) Hacer estimación para k=2:6 componentes
    #   2) Ordenar mixturas por número de componentes de mejor a peor BIC
    #   3) Tomar mixtura con mejor puntuación
    @rput coords
    R"""
    library(movMF)
    vMs = lapply(2:6, function(K) movMF(as.matrix(coords), k=K, control=list(nruns = 30)))
    options = sapply(vMs, BIC) # BIC para cada mixtura
    options_sorted = sort(options,index.return=TRUE)
    id_sorted = options_sorted$ix # El primer valor son los componentes-1 de la mejor mixtura
    vM = movMF(as.matrix(coords), k=id_sorted[1]+1, control=list(nruns = 30)) # Obtenemos la mejor mixtura
    """
    @rget vM

    Fn = getFn(radians)
    mixtura = getParams(vM)

    # Comprobar que sea posible el calculo de F0 (kappa<=700)
    i=1
    iter=1
    while (i <= size(mixtura)[1])
        if (mixtura[i,4] > 700)
            # Ninguna mixtura válida
            if (iter==5)
                print("Limit exceeded")
                return changeParams(0)
            end
            # Cambiamos a la siguiente mixtura con mejor BIC y reiniciamos
            mixtura = changeParams(iter)         
            i=1
            iter+=1
        else
            i+=1
        end

    end
    # Actual mejor mixtura calculable
    save = mixtura

    # Una vez encontrada una mixtura adecuada,
    # Obtenemos frecuencias acumuladas bajo parámetros estimados
    F0 = getF0(radians, mixtura)

    # Test de bondad de ajuste Kolmogorov-Smirnov
    while (!runKS(Fn,F0))
        if (iter==5)
            print("Limit exceeded")
            return save
        end

        mixtura=changeParams(iter)
        iter+=1

        # Buscamos un kappa < 700
        i=1
        while (i <= size(mixtura)[1])
            if (mixtura[i,4] > 700)
                # Ninguna mixtura válida
                if (iter==5)
                    print("Limit exceeded")
                    return save
                end
                # Cambiamos a la siguiente mixtura con mejor BIC y reiniciamos
                mixtura = changeParams(iter)         
                i=1
                iter+=1
            else
                i+=1
            end
        end

        F0 = getF0(radians, mixtura)
    end
    
    if runKS(Fn,F0)
        return mixtura
    else
        # En caso de no pasar los test, almacenamos la mixtua con mejor puntuación y calculable
        return save
    end

end


function changeParams(iter)

    @rput iter
    R"""
    vM = movMF(as.matrix(coords), k=id_sorted[1+iter]+1, control=list(nruns = 30))
    """
    @rget vM
    return getParams(vM)

end

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# -------------------------- ALMACENAR PARÁMETROS Von Mises------------------------
# ---------------------------------------------------------------------------------

function storeParams(mixtura,name)
    alpha = string(mixtura[:,1])
    kappa = string(mixtura[:,4])

    filename = "Params/"*name*".txt" 
    file = open(filename,"w")

    write(file, "ALPHA:\n")
    write(file, alpha*"\n")
    write(file, "KAPPA:\n")
    write(file,kappa*"\n")
    for i in 1:size(mixtura)[1]
        mu = string(Matrix(mixtura)[i,2:3])
        write(file, "MU"*string(i)*":\n")
        write(file, mu*"\n")
        theta = string(Matrix(mixtura)[i,5:6])
        write(file, "THETA"*string(i)*":\n")
        write(file, theta*"\n")
    end

    close(file)
end

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# ---------------------------------- EJECUTAR TEST --------------------------------
# ---------------------------------------------------------------------------------
using LinearAlgebra, Distributions

coord2rad()

@rget dormitorio_bano_radians
@rget dormitorio_cocina_radians
@rget dormitorio_salon_radians
@rget dormitorio_tv_radians

@rget bano_dormitorio_radians
@rget bano_cocina_radians
@rget bano_salon_radians
@rget bano_tv_radians

@rget cocina_dormitorio_radians
@rget cocina_bano_radians
@rget cocina_salon_radians
@rget cocina_tv_radians

@rget salon_dormitorio_radians
@rget salon_bano_radians
@rget salon_cocina_radians
@rget salon_tv_radians

@rget tv_dormitorio_radians
@rget tv_bano_radians
@rget tv_cocina_radians
@rget tv_salon_radians

# Ordenamos
sort!(dormitorio_bano_radians)
sort!(dormitorio_cocina_radians)
sort!(dormitorio_salon_radians)
sort!(dormitorio_tv_radians)

sort!(bano_dormitorio_radians)
sort!(bano_cocina_radians)
sort!(bano_salon_radians)
sort!(bano_tv_radians)

sort!(cocina_dormitorio_radians)
sort!(cocina_bano_radians)
sort!(cocina_salon_radians)
sort!(cocina_tv_radians)

sort!(salon_dormitorio_radians)
sort!(salon_bano_radians)
sort!(salon_cocina_radians)
sort!(salon_tv_radians)

sort!(tv_dormitorio_radians)
sort!(tv_bano_radians)
sort!(tv_cocina_radians)
sort!(tv_salon_radians)


# En este momento tenemos las transiciones generadas y ordenadas en radianes

# Generar parametros y testearlos
dormitorio_bano_mixtura = testParams(dormitorio_bano,dormitorio_bano_radians)
dormitorio_cocina_mixtura = testParams(dormitorio_cocina,dormitorio_cocina_radians)
dormitorio_salon_mixtura = testParams(dormitorio_salon,dormitorio_salon_radians)
dormitorio_tv_mixtura = testParams(dormitorio_tv,dormitorio_tv_radians)

storeParams(dormitorio_bano_mixtura,"dormitorio_bano")
storeParams(dormitorio_cocina_mixtura,"dormitorio_cocina")
storeParams(dormitorio_salon_mixtura,"dormitorio_salon")
storeParams(dormitorio_tv_mixtura,"dormitorio_tv")

bano_dormitorio_mixtura = testParams(bano_dormitorio,bano_dormitorio_radians)
bano_cocina_mixtura = testParams(bano_cocina,bano_cocina_radians)
bano_salon_mixtura = testParams(bano_salon,bano_salon_radians)
bano_tv_mixtura = testParams(bano_tv,bano_tv_radians)

storeParams(bano_dormitorio_mixtura,"bano_dormitorio")
storeParams(bano_cocina_mixtura,"bano_cocina")
storeParams(bano_salon_mixtura,"bano_salon")
storeParams(bano_tv_mixtura,"bano_tv")

cocina_dormitorio_mixtura = testParams(cocina_dormitorio,cocina_dormitorio_radians)
cocina_bano_mixtura = testParams(cocina_bano,cocina_bano_radians)
cocina_salon_mixtura = testParams(cocina_salon,cocina_salon_radians)
cocina_tv_mixtura = testParams(cocina_tv,cocina_tv_radians)

storeParams(cocina_dormitorio_mixtura,"cocina_dormitorio")
storeParams(cocina_bano_mixtura,"cocina_bano")
storeParams(cocina_salon_mixtura,"cocina_salon")
storeParams(cocina_tv_mixtura,"cocina_tv")

salon_dormitorio_mixtura = testParams(salon_dormitorio,salon_dormitorio_radians)
salon_cocina_mixtura = testParams(salon_cocina,salon_cocina_radians)
salon_bano_mixtura = testParams(salon_bano,salon_bano_radians)
salon_tv_mixtura = testParams(salon_tv,salon_tv_radians)

storeParams(salon_dormitorio_mixtura,"salon_dormitorio")
storeParams(salon_cocina_mixtura,"salon_cocina")
storeParams(salon_bano_mixtura,"salon_bano")
storeParams(salon_tv_mixtura,"salon_tv")

tv_dormitorio_mixtura = testParams(tv_dormitorio,tv_dormitorio_radians)
tv_cocina_mixtura = testParams(tv_cocina,tv_cocina_radians)
tv_bano_mixtura = testParams(tv_bano,tv_bano_radians)
tv_salon_mixtura = testParams(tv_salon,tv_salon_radians)

storeParams(tv_dormitorio_mixtura,"tv_dormitorio")
storeParams(tv_cocina_mixtura,"tv_cocina")
storeParams(tv_salon_mixtura,"tv_salon")
storeParams(tv_bano_mixtura,"tv_bano")

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
