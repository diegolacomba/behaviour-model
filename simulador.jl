using CSV, DataFrames, RCall, LinearAlgebra, Distributions, StatsBase
R"""
library(movMF)
library(circular)
"""


# Dormitorio:   1
# Baño:         2
# Cocina:       3
# Salon:        4
# Tv:           5


# ----------------- Leer los parámetros estimados con el entrenamiento -------------------------------------------------------------------

function readParams(name)
    filename = "Params/"*name*".txt"
    file = open(filename,"r")
    params = split(read(file,String),"\n")
    close(file)

    alpha = split(strip(params[2], ['[',']']),", ")
    kappa = split(strip(params[4], ['[',']']),", ")

    mixtura = DataFrame(alpha = parse.(Float64,alpha), mu_x = 0.0, mu_y = 0.0, kappa = parse.(Float64,kappa), theta_x = 0.0, theta_y = 0.0)

    for i in 1:size(mixtura)[1]

        mu = split(strip(params[2+(4*i)], ['[',']']),", ")
        mixtura[i,2:3] = parse.(Float64,mu)
        theta = split(strip(params[4+(4*i)], ['[',']']),", ")
        mixtura[i,5:6] = parse.(Float64,theta)

    end

    return mixtura
end

dormitorio_bano = readParams("dormitorio_bano")
dormitorio_cocina = readParams("dormitorio_cocina")
dormitorio_salon = readParams("dormitorio_salon")
dormitorio_tv = readParams("dormitorio_tv")

cocina_dormitorio = readParams("cocina_dormitorio")
cocina_bano = readParams("cocina_bano")
cocina_salon = readParams("cocina_salon")
cocina_tv = readParams("cocina_tv")

bano_dormitorio = readParams("bano_dormitorio")
bano_cocina = readParams("bano_cocina")
bano_salon = readParams("bano_salon")
bano_tv = readParams("bano_tv")

salon_dormitorio = readParams("salon_dormitorio")
salon_cocina = readParams("salon_cocina")
salon_bano = readParams("salon_bano")
salon_tv = readParams("salon_tv")

tv_dormitorio = readParams("tv_dormitorio")
tv_cocina = readParams("tv_cocina")
tv_salon = readParams("tv_salon")
tv_bano = readParams("tv_bano")

# -----------------------------------------------------------------------------------------------------------------------


# ------------------------ Variables y métodos para generar una secuencia de N dias -------------------------------------

struct Node
    id::Int8
    mixturas::Vector{DataFrame}
    destinos::Vector{Int8}
end

dormitorio = Node(
    1,
    [dormitorio_bano,dormitorio_cocina,dormitorio_salon,dormitorio_tv],
    [2,3,4,5]
)
bano = Node(
    2,
    [bano_dormitorio,bano_cocina,bano_salon,bano_tv],
    [1,3,4,5]
)
cocina = Node(
    3,
    [cocina_dormitorio,cocina_bano,cocina_salon,cocina_tv],
    [1,2,4,5]
)
salon = Node(
    4,
    [salon_dormitorio,salon_bano,salon_cocina,salon_tv],
    [1,2,3,5]
)
tv = Node(
    5,
    [tv_dormitorio,tv_bano,tv_cocina,tv_salon],
    [1,2,3,4]
)


# Probabilidades iniciales
rooms = [1,2,3,4,5]
initial_prob = Weights([0.65,0.1,0.1,0.05,0.1])
nodes=[dormitorio,bano,cocina,salon,tv]


function simulate(theta,alpha)
    @rput theta
    @rput alpha
    R"""
    sample = coord2rad(rmovMF(1, theta, alpha))
    """
    @rget sample
    return sample
end


function generateDaySecuence(day)
    t_current = 0
    node_current = nodes[last(node_sec)]
    run = true
    # 1) Simular todas las transiciones desde current
    # 2) Escoger la menor que sea "mayor" que el instante de tiempo actual,
    #       en caso de ser todas menores, escoger la menor.

    while run
        options = []
        push!(options,simulate(Matrix(node_current.mixturas[1][:,5:6]),node_current.mixturas[1][:,:alpha]))
        push!(options,simulate(Matrix(node_current.mixturas[2][:,5:6]),node_current.mixturas[2][:,:alpha]))
        push!(options,simulate(Matrix(node_current.mixturas[3][:,5:6]),node_current.mixturas[3][:,:alpha]))
        push!(options,simulate(Matrix(node_current.mixturas[4][:,5:6]),node_current.mixturas[4][:,:alpha]))
        ids=sortperm(options)

        i=1
        while true
            # Si todas son menores
            if i == 5
                node_current = nodes[node_current.destinos[ids[1]]]
                t_current = options[ids[1]]

                push!(node_sec, node_current.id)
                push!(t_sec, t_current)
                push!(day_sec,day+1)
                run = false
                break
            end 

            if options[ids[i]]>t_current
                node_current = nodes[node_current.destinos[ids[i]]]
                t_current = options[ids[i]]

                push!(node_sec, node_current.id)
                push!(t_sec, t_current)
                push!(day_sec,day)
                break
            else
                i += 1
            end

        end
    end
end


function generateNDaysSecuence(days)
    push!(node_sec, sample(rooms,initial_prob))
    push!(t_sec,0)
    push!(day_sec, 1)

    for i in 1:days
        generateDaySecuence(i)
        i+=1
    end
end

# -----------------------------------------------------------------------------------------------------------------------




#---------------------------------------------------------------------------------------------
#---------------------------------- GENERAR SECUENCIA ----------------------------------------
#---------------------------------------------------------------------------------------------

t=2π/288

node_sec = []
t_sec = []
day_sec = []

generateNDaysSecuence(1000)
secuencia = DataFrame(dias=day_sec,nodos=node_sec,minutos=t_sec)
CSV.write("secuencia.csv", secuencia)


# ------------ Generar secuencia con cambio en los parámetros ------------------------------

# Generar secuencia sin cambios
generateNDaysSecuence(200)

# Introducir cambio en mixtura "dormitorio_bano"
# Concretamente en la media de un parámetro + 1rad

mixtura = copy(dormitorio.mixturas[1])
v = [mixtura[2,2],mixtura[2,3]]
reshape(v, 1, length(v))
@rput v
R"""
rad = coord2rad(t(as.matrix(v)))
"""
@rget rad

rad = rad+1
coords = (cos(rad),sin(rad))
mixtura[2,2] = coords[1]
mixtura[2,3] = coords[2]

dormitorio.mixturas[1] = mixtura

# Generar secuencia con cambio
generateNDaysSecuence(400)
secuencia = DataFrame(dias=day_sec,nodos=node_sec,minutos=t_sec)
CSV.write("secuencia.csv", secuencia)

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------




#---------------------------------------------------------------------------------------------
#----------------------------- ESTIMAR PARÁMETROS SECUENCIA ----------------------------------
#---------------------------------------------------------------------------------------------

# Obtener momentos de transición desde "origen" hasta "destino"
function transiciones(origen, destino)
    v = Float64[]

    for i in 1:nrow(secuencia)-1
        if secuencia[i,:nodos]==origen && secuencia[i+1,:nodos]==destino
            push!(v,secuencia[i+1,:minutos])
        end
    end

    return v
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


# Estimar mixtura de von Mises
function estimateParams(coords)
    @rput coords
    R"""
    vMs = lapply(2:6, function(K) movMF(as.matrix(coords), k=K, control=list(nruns = 30)))
    options = sapply(vMs, BIC) # BIC para cada mixtura
    options_sorted = sort(options,index.return=TRUE)
    id_sorted = options_sorted$ix # El primer valor son los componentes-1 de la mejor mixtura
    vM = movMF(as.matrix(coords), k=id_sorted[1]+1, control=list(nruns = 30)) # Obtenemos la mejor mixtura
    """
    @rget vM
    return getParams(vM)
end



# Transiciones en radianes

dormitorio_bano_transiciones = transiciones(1,2)
dormitorio_cocina_transiciones = transiciones(1,3)
dormitorio_salon_transiciones = transiciones(1,4)
dormitorio_tv_transiciones = transiciones(1,5)

bano_dormitorio_transiciones = transiciones(2,1)
bano_cocina_transiciones = transiciones(2,3)
bano_salon_transiciones = transiciones(2,4)
bano_tv_transiciones = transiciones(2,5)

cocina_dormitorio_transiciones = transiciones(3,1)
cocina_bano_transiciones = transiciones(3,2)
cocina_salon_transiciones = transiciones(3,4)
cocina_tv_transiciones = transiciones(3,5)

salon_dormitorio_transiciones = transiciones(4,2)
salon_bano_transiciones = transiciones(4,2)
salon_cocina_transiciones = transiciones(4,3)
salon_tv_transiciones = transiciones(4,5)

tv_dormitorio_transiciones = transiciones(5,1)
tv_bano_transiciones = transiciones(5,2)
tv_cocina_transiciones = transiciones(5,3)
tv_salon_transiciones = transiciones(5,4)



# Transiciones en coordenadas

dormitorio_bano_coords = DataFrame()
dormitorio_cocina_coords = DataFrame()
dormitorio_salon_coords = DataFrame()
dormitorio_tv_coords = DataFrame()

bano_dormitorio_coords = DataFrame()
bano_cocina_coords = DataFrame()
bano_salon_coords = DataFrame()
bano_tv_coords = DataFrame()

cocina_dormitorio_coords = DataFrame()
cocina_bano_coords = DataFrame()
cocina_salon_coords = DataFrame()
cocina_tv_coords = DataFrame()

salon_dormitorio_coords = DataFrame()
salon_bano_coords = DataFrame()
salon_cocina_coords = DataFrame()
salon_tv_coords = DataFrame()

tv_dormitorio_coords = DataFrame()
tv_bano_coords = DataFrame()
tv_cocina_coords = DataFrame()
tv_salon_coords = DataFrame()


# pasar a coordenadas y estimar parametros con movMF, luego comparar resultados
function genera_transiciones_dormitorio()

    # bano
    dormitorio_bano = rad2unitcircle(dormitorio_bano_transiciones)
    dormitorio_bano_coords.x = dormitorio_bano[1]
    dormitorio_bano_coords.y = dormitorio_bano[2]

    # COCINA
    dormitorio_cocina = rad2unitcircle(dormitorio_cocina_transiciones)
    dormitorio_cocina_coords.x = dormitorio_cocina[1]
    dormitorio_cocina_coords.y = dormitorio_cocina[2]

    # TELEVISION
    dormitorio_tv = rad2unitcircle(dormitorio_salon_transiciones)
    dormitorio_tv_coords.x = dormitorio_tv[1]
    dormitorio_tv_coords.y = dormitorio_tv[2]

    # SALON
    dormitorio_salon = rad2unitcircle(dormitorio_tv_transiciones)
    dormitorio_salon_coords.x = dormitorio_salon[1]
    dormitorio_salon_coords.y = dormitorio_salon[2]

end



# ------------------------------ BAÑO --------------------------------------
function genera_transiciones_bano()
    
    # DORMITORIO
    bano_dormitorio = rad2unitcircle(bano_dormitorio_transiciones)
    bano_dormitorio_coords.x = bano_dormitorio[1]
    bano_dormitorio_coords.y = bano_dormitorio[2]

    # COCINA
    bano_cocina = rad2unitcircle(bano_cocina_transiciones)
    bano_cocina_coords.x = bano_cocina[1]
    bano_cocina_coords.y = bano_cocina[2]

    # TELEVISION
    bano_tv = rad2unitcircle(bano_tv_transiciones)
    bano_tv_coords.x = bano_tv[1]
    bano_tv_coords.y = bano_tv[2]

    # SALON
    bano_salon = rad2unitcircle(bano_salon_transiciones)
    bano_salon_coords.x = bano_salon[1]
    bano_salon_coords.y = bano_salon[2]

end


# ------------------------------ COCINA --------------------------------------
function genera_transiciones_cocina()
    
    # DORMITORIO
    cocina_dormitorio = rad2unitcircle(cocina_dormitorio_transiciones)
    cocina_dormitorio_coords.x = cocina_dormitorio[1]
    cocina_dormitorio_coords.y = cocina_dormitorio[2]

    # bano
    cocina_bano = rad2unitcircle(cocina_bano_transiciones)
    cocina_bano_coords.x = cocina_bano[1]
    cocina_bano_coords.y = cocina_bano[2]

    # TELEVISION
    cocina_tv = rad2unitcircle(cocina_tv_transiciones)
    cocina_tv_coords.x = cocina_tv[1]
    cocina_tv_coords.y = cocina_tv[2]

    # SALON
    cocina_salon = rad2unitcircle(cocina_salon_transiciones)
    cocina_salon_coords.x = cocina_salon[1]
    cocina_salon_coords.y = cocina_salon[2]

end


# ------------------------------ TELEVISION --------------------------------------
function genera_transiciones_tv()
    
    # DORMITORIO
    tv_dormitorio = rad2unitcircle(tv_dormitorio_transiciones)
    tv_dormitorio_coords.x = tv_dormitorio[1]
    tv_dormitorio_coords.y = tv_dormitorio[2]

    # bano
    tv_bano = rad2unitcircle(tv_bano_transiciones)
    tv_bano_coords.x = tv_bano[1]
    tv_bano_coords.y = tv_bano[2]

    # COCINA
    tv_cocina= rad2unitcircle(tv_cocina_transiciones)
    tv_cocina_coords.x = tv_cocina[1]
    tv_cocina_coords.y = tv_cocina[2]

    # SALON
    tv_salon = rad2unitcircle(tv_salon_transiciones)
    tv_salon_coords.x = tv_salon[1]
    tv_salon_coords.y = tv_salon[2]

end


# ------------------------------ SALON --------------------------------------
function genera_transiciones_salon()
    
    # DORMITORIO
    salon_dormitorio = rad2unitcircle(salon_dormitorio_transiciones)
    salon_dormitorio_coords.x = salon_dormitorio[1]
    salon_dormitorio_coords.y = salon_dormitorio[2]

    # bano
    salon_bano = rad2unitcircle(salon_bano_transiciones)
    salon_bano_coords.x = salon_bano[1]
    salon_bano_coords.y = salon_bano[2]

    # COCINA
    salon_cocina = rad2unitcircle(salon_cocina_transiciones)
    salon_cocina_coords.x = salon_cocina[1]
    salon_cocina_coords.y = salon_cocina[2]

    # TELEVISION
    salon_tv = rad2unitcircle(salon_tv_transiciones)
    salon_tv_coords.x = salon_tv[1]
    salon_tv_coords.y = salon_tv[2]

end


# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

genera_transiciones_dormitorio()
genera_transiciones_bano()
genera_transiciones_cocina()
genera_transiciones_salon()
genera_transiciones_tv()

dormitorio_bano_fitted = estimateParams(dormitorio_bano_coords)
dormitorio_cocina_fitted = estimateParams(dormitorio_cocina_coords)
dormitorio_salon_fitted = estimateParams(dormitorio_salon_coords)
dormitorio_tv_fitted = estimateParams(dormitorio_tv_coords)

cocina_dormitorio_fitted = estimateParams(cocina_dormitorio_coords)
cocina_bano_fitted = estimateParams(cocina_bano_coords)
cocina_salon_fitted = estimateParams(cocina_salon_coords)
cocina_tv_fitted = estimateParams(cocina_tv_coords)

bano_dormitorio_fitted = estimateParams(bano_dormitorio_coords)
bano_cocina_fitted = estimateParams(bano_cocina_coords)
bano_salon_fitted = estimateParams(bano_salon_coords)
bano_tv_fitted = estimateParams(bano_tv_coords)

salon_dormitorio_fitted = estimateParams(salon_dormitorio_coords)
salon_cocina_fitted = estimateParams(salon_cocina_coords)
salon_bano_fitted = estimateParams(salon_bano_coords)
salon_tv_fitted = estimateParams(salon_tv_coords)

tv_dormitorio_fitted = estimateParams(tv_dormitorio_coords)
tv_cocina_fitted = estimateParams(tv_cocina_coords)
tv_salon_fitted = estimateParams(tv_salon_coords)
tv_bano_fitted = estimateParams(tv_bano_coords)


dormitorio_salon
dormitorio_salon_fitted

dormitorio_tv
dormitorio_tv_fitted

cocina_dormitorio
cocina_dormitorio_fitted


