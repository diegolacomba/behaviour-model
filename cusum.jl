using DataFrames, CSV, RCall, LinearAlgebra, Plots

R"""
library(movMF)
library(circular)
"""

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


# -----------------------------------------------------------------------------------------------------------------
# Lectura de parámetros
# -----------------------------------------------------------------------------------------------------------------
transicion = readParams("salon_cocina")
theta = Matrix(transicion[:,5:6])
alpha = transicion[:,:alpha]



# -----------------------------------------------------------------------------------------------------------------
# Cambio en los parámetros
# -----------------------------------------------------------------------------------------------------------------
theta_mod = copy(theta)
alpha_mod = copy(alpha)

# Cambio en mu de una componente
mu = [transicion[1,2],transicion[1,3]]
reshape(mu, 1, length(mu))
@rput mu
R"""
rad = coord2rad(t(as.matrix(mu)))
"""
@rget rad

rad = rad+0.3
coords = [cos(rad),sin(rad)]

theta_mod[1,:] = coords.*transicion[1,4]


# Cambio en kappa de una componente
mu = [transicion[1,2],transicion[1,3]]
kappa = transicion[1,4]

kappa = kappa+5

theta_mod[1,:] = mu.*kappa


# Cambio en mu y kappa de una componente
theta_mod[1,:] = coords.*kappa


# Cambio en las proporciones (alpha)
alpha_mod[1] = 0.5
alpha_mod[2] = 0.25
alpha_mod[3] = 0.25




theta
theta_mod

alpha
alpha_mod


# -----------------------------------------------------------------------------------------------------------------
# Simulamos sin y con cambio
# -----------------------------------------------------------------------------------------------------------------
@rput theta
@rput theta_mod
@rput alpha
@rput alpha_mod
R"""
sample = rmovMF(15, theta, alpha)
sample_mod = rmovMF(50, theta_mod, alpha_mod)
"""
@rget sample
@rget sample_mod

muestra_coords = vcat(sample,sample_mod)

#Sin cambio
muestra_coords = sample

# ------------------------- CUSUM ------------------------------------

# Obtener EMV del vector de parámetros para cambios en j = 1,...,N-40 con kappa < 700.

# Se descarta la posibilidad de cambio en las 40 ultimas observaciones para
# obtener una correcta estimación de los parámetros. Es decir, para tener un tamaño
# muestral asequible en todos los casos.
# Tendremos un total de N-40 EMVs.

# Se descarta la posibilidad de cambios en el numero de 
#componentes de la mixtura.

EMVs = []
tam = length(muestra_coords[:,1])
n = tam-40
componentes = nrow(transicion)
@rput componentes

# Tamaño de muestra mínimo = 40
for j in 1:n
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
CumSums = []

# Calculo del estadístico de la razón de verosimilitud generalizado 
# para cada contraste
for j in 1:n
    if EMVs[j] != 0
        muestra = muestra_coords[j:tam,:]

        theta1 = Matrix(EMVs[j][:,5:6])
        alpha1 = EMVs[j][:,:alpha]

        @rput muestra
        @rput theta1
        @rput alpha1
        R"""
        p0 = dmovMF(muestra, theta, alpha)
        p1 = dmovMF(muestra, theta1, alpha1)
        """
        @rget p0
        @rget p1

        cumsum = 2*sum(log.(p1)-log.(p0))

        push!(CumSums,cumsum)

    else
        push!(CumSums,-1)
    end
end

# Aquí podemos ver que el valor máximo se corresponde a la suma acumulada con el punto de cambio
# muy cercano al real.
show(stdout, "text/plain", CumSums)


# Plot CumSums
x=range(1,n,length=n)
plot(x,CumSums,title="",label=false)

savefig("cumsum_25_a05-025-025.png")


# Estadístico de contraste (supremo)
τ = sortperm(CumSums,rev=true)[1]
Λ = CumSums[τ]

# Cálculo del umbral de rechazo
α = 0.05
prob = α/n
p = componentes+(componentes-1)
@rput prob
@rput p
R"""
umbral = qchisq(p=prob, df=p, lower.tail=FALSE)
"""
@rget umbral

# --------------------------------------------------------------------------------------------
