using DataFrames, CSV, RCall, LinearAlgebra, Plots, Statistics, BenchmarkTools, JLD2

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

componentes = nrow(transicion)

theta_mod = copy(theta)
alpha_mod = copy(alpha)

# -----------------------------------------------------------------------------------------------------------------
# Cambio en la media de una componente
# -----------------------------------------------------------------------------------------------------------------
function modify_mu(componente, cambio)

    # Cambio en mu de 1ra componente
    if componente >= 1 && componente <= componentes
        mu = [transicion[componente,2],transicion[componente,3]]
        reshape(mu, 1, length(mu))
        @rput mu
        R"""
        rad = coord2rad(t(as.matrix(mu)))
        """
        @rget rad

        rad = rad+cambio
        coords = [cos(rad),sin(rad)]

        global theta_mod[componente,:] = coords.*transicion[componente,4]
    
    else
        throw(DomainError(componente, "argument must be between 1 and componentes"))
    end

end



# -----------------------------------------------------------------------------------------------------------------
# Cambio en las proporciones de cada componente .5-.25-.25
# -----------------------------------------------------------------------------------------------------------------
function modify_comp_prop(c1,c2,c3)
    
    global alpha_mod = [c1,c2,c3]
    
end





# -----------------------------------------------------------------------------------------------------------------
# Realizar las pruebas un numero considerable de veces
# -----------------------------------------------------------------------------------------------------------------

function execute_tests(n_test)

    @rput theta
    @rput theta_mod
    @rput alpha
    @rput alpha_mod

    @rput tam
    @rput cambio
    @rput componentes

    errors = []

    for iter in 1:n_test
        
        # -----------------------------------------------------------------------------------------------------------------
        # Simulamos
        # -----------------------------------------------------------------------------------------------------------------
        R"""
        sample = rmovMF(cambio, theta, alpha)
        sample_mod = rmovMF(tam-cambio, theta_mod, alpha_mod)
        """
        @rget sample
        @rget sample_mod

        muestra_coords = vcat(sample,sample_mod)



        # -----------------------------------------------------------------------------------------------------------------
        #  C U S U M
        # -----------------------------------------------------------------------------------------------------------------

        # Se descarta la posibilidad de cambio en las 40 ultimas observaciones para obtener una correcta estimación de los 
        # parámetros. Es decir, un tamaño muestral asequible en todos los casos (limitacion EM).
        # Tendremos un total de N-40 EMVs.
        # Se descarta la posibilidad de cambios en el numero de componentes de la mixtura.

        # Obtener EMV del vector de parámetros para cambios en j = 1,...,N-40 con kappa < 700.
        EMVs = []

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

        # Sumas acumuladas
        CumSums = []

        # Calculo del estadístico de la razón de verosimilitud generalizado para cada contraste
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

        #show(stdout, "text/plain", CumSums)

        # Plot & save CumSums
        x=range(1,n,length=n)
        fig = plot(x,CumSums,title="",label=false)

        file = ruta*string(iter)*".png"
        savefig(fig, file)

        # Estadístico de contraste (supremo)
        τ = sortperm(CumSums,rev=true)[1]
        Λ = CumSums[τ]

        if Λ > umbral
            push!(errors,abs(cambio - τ))
        end

    end

    return errors

end



# -----------------------------------------------------------------------------------------------------------------
#  T A M A Ñ O    M U E S T R A L   50   -   C A M B I O   E N   O B S E R V A C I O N   35
# -----------------------------------------------------------------------------------------------------------------

n = 50
tam = n+40
cambio = 35

# Umbral de rechazo
α = 0.05
prob = α/n
p = componentes+(componentes-1)
@rput prob
@rput p
R"""
umbral = qchisq(p=prob, df=p, lower.tail=FALSE)
"""
@rget umbral

# Cambio leve en media 1ra componente
ruta = "Resultados cusum/50_mu03/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.3)

errors = execute_tests(100)
jldsave("errors_50_mu03.jld2"; errors)

aux = load("errors_50_mu03.jld2")
errors = aux["errors"]
mean(errors)


# Cambio leve en media 2 componentes (1 y 2)
ruta = "Resultados cusum/50_1mu03_2mu03/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.3)
modify_mu(2,0.3)

errors = execute_tests(100)
jldsave("errors_50_1mu03_2mu03.jld2"; errors)

aux = load("errors_50_1mu03_2mu03.jld2")
errors = aux["errors"]
mean(errors)



# Cambio leve en media 3 componentes
ruta = "Resultados cusum/50_1mu03_2mu03_3mu03/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.3)
modify_mu(2,0.3)
modify_mu(3,0.3)

errors = execute_tests(100)
jldsave("errors_50_1mu03_2mu03_3mu03.jld2"; errors)

aux = load("errors_50_1mu03_2mu03_3mu03.jld2")
errors = aux["errors"]
mean(errors)

# Cambio grande en media 1ra componente
ruta = "Resultados cusum/50_mu08/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.8)

errors = execute_tests(100)
jldsave("errors_50_mu08.jld2"; errors)



# Cambio en las proporciones
ruta = "Resultados cusum/50_a05-025-025/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_comp_prop(0.5,0.25,0.25)

errors = execute_tests(100)
jldsave("errors_50_a05-025-025.jld2"; errors)

aux = load("errors_50_a05-025-025.jld2")
errors = aux["errors"]
mean(errors)

# Sin cambio
ruta = "Resultados cusum/50/"

theta_mod = copy(theta)
alpha_mod = copy(alpha)

errors = execute_tests(100)
jldsave("errors_50.jld2"; errors)

aux = load("errors_50.jld2")
errors = aux["errors"]
mean(errors)



# -----------------------------------------------------------------------------------------------------------------
#  T A M A Ñ O    M U E S T R A L   100   -   C A M B I O   E N   O B S E R V A C I O N   50
# -----------------------------------------------------------------------------------------------------------------

n = 100
tam = n+40
cambio = 50

# Umbral de rechazo
α = 0.05
prob = α/n
p = componentes+(componentes-1)
@rput prob
@rput p
R"""
umbral = qchisq(p=prob, df=p, lower.tail=FALSE)
"""
@rget umbral

# Cambio leve (0.3 rad) en media 1ra componente
ruta = "Resultados cusum/100_mu03/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.3)

errors = execute_tests(100)
jldsave("errors_100_mu03.jld2"; errors)



# Cambio leve en media 2 componentes (1 y 2)
ruta = "Resultados cusum/100_1mu03_2mu03/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.3)
modify_mu(2,0.3)

errors = execute_tests(100)
jldsave("errors_100_1mu03_2mu03.jld2"; errors)

aux = load("errors_100_1mu03_2mu03.jld2")
errors = aux["errors"]

mean(errors)



# Cambio leve en media 3 componentes
ruta = "Resultados cusum/100_1mu03_2mu03_3mu03/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.3)
modify_mu(2,0.3)
modify_mu(3,0.3)

errors = execute_tests(100)
jldsave("errors_100_1mu03_2mu03_3mu03.jld2"; errors)

aux = load("errors_100_1mu03_2mu03_3mu03.jld2")
errors = aux["errors"]

mean(errors)

# Cambio grande en media
ruta = "Resultados cusum/100_mu08/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_mu(1,0.8)

errors = execute_tests(100)
jldsave("errors_100_mu08.jld2"; errors)

aux = load("errors_100_mu08.jld2")
errors = aux["errors"]
mean(errors)

# Cambio en las proporciones
ruta = "Resultados cusum/100_a05-025-025/"
theta_mod = copy(theta)
alpha_mod = copy(alpha)

modify_comp_prop(0.5,0.25,0.25)

errors = execute_tests(100)
jldsave("errors_100_a05-025-025.jld2"; errors)

aux = load("errors_100_a05-025-025.jld2")
errors = aux["errors"]
mean(errors)

# Sin cambio
ruta = "Resultados cusum/100/"

theta_mod = copy(theta)
alpha_mod = copy(alpha)

errors = execute_tests(100)
jldsave("errors_100.jld2"; errors)

aux = load("errors_100.jld2")
errors = aux["errors"]
mean(errors)