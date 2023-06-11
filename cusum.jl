using DataFrames, CSV, RCall, LinearAlgebra

R"""
library(movMF)
library(circular)
"""

# 1ro) Escoger una transicion para introducir el cambio
# 2do) Simular secuencia con un cambio en las parámetros de la mixtura correspondiente en un instante de tiempo determinado
#   (desde simulador en un csv)
# 3ro) Leer secuencia simulada y obtener transiciones de la mixtura correspondiente
# 4to) CUSUM

secuencia = CSV.read("secuencia.csv",DataFrame)

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

transitions = transiciones(1, 2)


# Otra forma para comprobar la correcta implementación de CUSUM
# Simular directamente de la mixtura escogida

# Leemos los parámetros de la mixtura
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

dormitorio_bano = readParams("dormitorio_bano")
theta = Matrix(dormitorio_bano[:,5:6])
alpha = dormitorio_bano[:,:alpha]
mu = [dormitorio_bano[1,2],dormitorio_bano[1,3]]


# Cambio en mu
reshape(mu, 1, length(mu))
@rput mu
R"""
rad = coord2rad(t(as.matrix(mu)))
"""
@rget rad

rad = rad+3
coords = [cos(rad),sin(rad)]

theta_mod = coords.*dormitorio_bano[1,4]

dormitorio_bano_mod = copy(dormitorio_bano)
dormitorio_bano_mod[1,2]=coords[1]
dormitorio_bano_mod[1,3]=coords[2]
dormitorio_bano_mod[1,5]=theta_mod[1]
dormitorio_bano_mod[1,6]=theta_mod[2]

componentes = nrow(dormitorio_bano)

# Simulamos sin y con cambio
@rput theta
@rput theta_mod
@rput alpha
R"""
sample = rmovMF(65, theta, alpha)
sample_mod = rmovMF(65, theta_mod, alpha)
rads = coord2rad(sample)
rads_mod = coord2rad(sample_mod)
"""
@rget sample
@rget sample_mod
@rget rads
@rget rads_mod

muestra_coords = vcat(sample,sample_mod)
muestra_rads = vcat(rads,rads_mod)
#muestra_coords = sample
#muestra_rads = rads
# ------------------------- CUSUM ------------------------------------

# Obtener EMV del vector de parámetros para cambios en j = 1,...,N-40 con kappa < 700.

# Se descarta la posibilidad de cambio en las 40 ultimas observaciones para
# obtener una correcta estimación de los parámetros. Es decir, para tener un tamaño
# muestral asequible en todos los casos.
# Tendremos un total de N-40 EMVs.

# Se descarta la posibilidad de cambios en el numero de componentes de la mixtura.

EMVs = []
tam = length(muestra_rads)

@rput componentes

# Tamaño de muestra mínimo = 40
for j in 1:tam-40
    coords = muestra_coords[j:tam,:]
    @rput coords
    try
        R"""
        vM = movMF(coords, k=componentes, control=list(nruns = 30))
        """
        @rget vM
        mixtura = getParams(vM)

        push!(EMVs,mixtura)
    catch e
        print("EM no converge\n")
        push!(EMVs,0)
    end
    
end

# Obtener todas las sumas acumuladas 
EMVs
CumSums = []

theta0 = Matrix(dormitorio_bano[:,5:6])
alpha0 = dormitorio_bano[:,:alpha]

@rput theta0
@rput alpha0

for j in 1:tam-40
    if EMVs[j] != 0
        muestra = muestra_coords[j:tam,:]

        theta1 = Matrix(EMVs[j][:,5:6])
        alpha1 = EMVs[j][:,:alpha]

        @rput muestra
        @rput theta1
        @rput alpha1
        R"""
        p0 = dmovMF(muestra, theta0, alpha0)
        p1 = dmovMF(muestra, theta1, alpha1)
        """
        @rget p0
        @rget p1

        cumsum = sum(log.(p1)-log.(p0))

        push!(CumSums,cumsum)

    else
        push!(CumSums,-1)
    end
end


# Aquí podemos ver que el valor máximo se corresponde a la suma acumulada con el punto de cambio
# muy cercano al real.
show(stdout, "text/plain", CumSums)

using Plots

x=range(1,70,length=70)
plot(x,CumSums,title="",label=false)
savefig("ratios-log-ver.png")


# Tomar el maximo, es decir, en el que ocurre más veces que p0 < p1, o dicho de otra forma,
# cuando el punto de cambio es más acertado.
G_emv = sortperm(CumSums,rev=true)[1]
G = CumSums[G_emv]

#-----------------------------------------------------------------------------------
#----------------- Ver evolución de la suma acumulada maxima -----------------------
#-----------------------------------------------------------------------------------
theta1 = Matrix(EMVs[G_emv][:,5:6])
alpha1 = EMVs[G_emv][:,:alpha]

cumsum = []

@rput muestra_coords
@rput theta1
@rput alpha1
R"""
p0 = dmovMF(muestra_coords, theta0, alpha0)
p1 = dmovMF(muestra_coords, theta1, alpha1)
"""
@rget p0
@rget p1

s = log.(p1)-log.(p0)
S = s[1]
push!(cumsum,S)
for i in 2:length(s)
    S = S+s[i]
    push!(cumsum,S)
end
cumsum
sum(log.(p1)-log.(p0))

x=range(1,110,length=110)
plot(x,cumsum,title="Evolución ratio log-verosimilitud máximo",label=false)
savefig("ratio-log-ver-max.png")

#-------------------------------------------------------------------------------------
#= 
CumSums_total = []
@rput muestra_coords
for j in 1:tam-40
    if EMVs[j] != 0

        theta1 = Matrix(EMVs[j][:,5:6])
        alpha1 = EMVs[j][:,:alpha]

        @rput theta1
        @rput alpha1
        R"""
        p0 = dmovMF(muestra_coords, theta0, alpha0)
        p1 = dmovMF(muestra_coords, theta1, alpha1)
        """
        @rget p0
        @rget p1

        s = log.(p1)-log.(p0)
        S = s[1]
        print(S)
        print("\n")
        for i in 2:length(s)
            S = S+s[i]
            print(S)
            print("\n")
        end
        push!(CumSums_total,S)
    else
        print("Invalid")
        push!(CumSums_total,-1)
    end
    print("-----------------------------")
end
show(stdout, "text/plain", CumSums_total) =#

dormitorio_bano
EMVs[G_emv]
# Obtener el umbral para ceptar el cambio (Bootstrap)
using Bootstrap,Statistics
n_boot = 1000

bootstrap(maximum, )

# Sacar con reemplazamiento N-40-4 y capturar G 100 veces

muestra = muestra_coords[1:tam,:]

theta1 = Matrix(EMVs[1][:,5:6])
alpha1 = EMVs[1][:,:alpha]

@rput muestra
@rput theta1
@rput alpha1
R"""
p0 = dmovMF(muestra, theta0, alpha0)
p1 = dmovMF(muestra, theta1, alpha1)
"""
@rget p0
@rget p1

a = log.(p1)-log.(p0)
show(stdout, "text/plain", a)

cumsum = sum(log.(p1)-log.(p0))

push!(CumSums,cumsum)