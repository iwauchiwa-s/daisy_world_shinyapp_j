library("shiny")
library("ggplot2")

# Daisy world simulator by S. Sugawara

shinyServer(function(input, output) {
  
  # constants
  t_k = 273  # kelvine
  fct = 20   # constant for local temperature calc
  pb1 = t_k + 0 # lower limit temperature for growth
  pb2 = t_k + 30 # upper limit temperature for growth
  dr = 0.2  # death rate
  a_w0 = 0.1 # initial area of white daysies
  a_b0 = 0.1 # initial area of black daisies
  a_a0 = 1.0 - a_w0 - a_b0
  a_t = 1.0	 # total area (all area denotes propotion)
  sbc = 5.67e-8	 # Stefan-Boltzman constant
  sc0 = 1380	# present solar constant (W m-2)
  nclc = 500   # integration cycle number

  # Initial area
  a_w = a_w0	# area of white daysies
  a_b = a_b0	# area of black daisies
  

  
  mydata <- reactive({
    # Model Parameters
    r_a <- input$Ra  # alebedo for ground
    r_w <- input$Rw  # albedo for white
    r_b <- input$Rb  # albedo for black
    
    if( input$ssys == 1){
    	scf <- scf0 <- input$Scfc # solar factor(Constant scenario)
    	scstp = 0
    }
    if( input$ssys == 2){
    	scf <- scf1 <- input$Scf1 # Initial solar constant factor (Linear scenario)
    	scstp <- input$Scstp  # Change rate (per kilo-year) (Linear scenario)
    }
    if( input$ssys == 3){
    	scf_amp <- input$Scf_amp # amplitude (Periodic scenario)
    	scf_frq <- input$Scf_frq  # Period (kilo-year) (Periodic scenario)
    	scf <- 1.0
    }
    
    # Initial values
    solar_fct <- scf
    solar_c <- sc <- sc0 * scf
    area_white <- a_w
    area_black <- a_b
    area_ground <- a_a <- a_t - a_w - a_b # available area
    alb_planet <- r_t <- a_a * r_a + a_w * r_w + a_b * r_b
    temp_eq <- t_e <- ( ( 1 - r_t ) * sc / 2 / sbc ) ^ 0.25
    temp_eqn <- t_en <- ( ( 1 - r_a ) * sc / 2 / sbc ) ^ 0.25
    temp_w <- t_w <- t_e + fct * ( r_t - r_w )
    temp_b <- t_b <- t_e + fct * ( r_t - r_b )
    
	# daisy growth rate (temperature-dependent model)(/killo year)
	if((t_w < pb1 ) || (t_w > pb2)){
	  g_w = 0
	} else{
	  g_w = 1 - ( ( 2 / ( pb2 - pb1 ) ) * ( t_w - ( pb1 + pb2 ) / 2 ) ) ^ 2
	}

	if((t_b < pb1) || (t_b > pb2)) {
	  g_b = 0
	} else{
	  g_b = 1 - ( ( 2 / ( pb2 - pb1 ) ) * ( t_b - ( pb1 + pb2 ) / 2 ) ) ^ 2
	}


    # Loop through periods
    for (p in 1:nclc) {
	  # change of solar const.
	  if( input$ssys == 1 || input$ssys == 2){
	  	scf = solar_fct[1] + p * scstp # constant & linear change
	  }else{
	  	scf = 1.0 + scf_amp * sin (2 * p * 3.1415 / scf_frq ) # periodic change
	  }
	  sc = sc0 * scf
	  
	  # change of area proportion
	  a_w = a_w + a_w * a_a * g_w - a_w * dr
	  a_b = a_b + a_b * a_a * g_b - a_b * dr
	  a_a = a_t - a_w - a_b
	  
      # planetary albedo
      r_t = a_a * r_a + a_w * r_w + a_b * r_b
      
	  # radiative equilibrium temperature assuming 1-layer atmosphere (K)
	  t_e = ( ( 1 - r_t ) * sc / 2 / sbc ) ^ 0.25
	  # radiative equilibrium temperature without daisy
	  t_en = ( ( 1 - r_a ) * sc / 2 / sbc ) ^ 0.25

	  #local temperature calc (albedo-linear dependent model)(K)
	  t_w = t_e + fct * ( r_t - r_w )
	  t_b = t_e + fct * ( r_t - r_b )

	　# daisy growth rate (temperature-dependent model)(/killo year)
	  if((t_w < pb1 ) || (t_w > pb2)){
		g_w = 0
	  } else{
		g_w = 1 - ( ( 2 / ( pb2 - pb1 ) ) * ( t_w - ( pb1 + pb2 ) / 2 ) ) ^ 2
	  }

	  if((t_b<pb1) || (t_b>pb2)) {
		g_b = 0
	  } else{
		g_b = 1 - ( ( 2 / ( pb2 - pb1 ) ) * ( t_b - ( pb1 + pb2 ) / 2 ) ) ^ 2
	  }

      # Save the changes in vector form
      solar_c = c(solar_c, sc)
      solar_fct = c(solar_fct, scf)
      area_white = c(area_white, a_w)
      area_black = c(area_black, a_b)
      area_ground = c(area_ground, a_a)
      alb_planet = c(alb_planet, r_t)
      temp_eq = c(temp_eq, t_e)
      temp_eqn = c(temp_eqn, t_en)
      temp_w = c(temp_w, t_w)
      temp_b = c(temp_b, t_b)
            
    }
    
    # Turn the results into data groups
    #_Temp
    temp <- data.frame(
      Time=rep((1:length(temp_eq)),2), 
      Temp = c(temp_eq, temp_eqn), 
      World=rep(c("daisy_world", "no_daisy_world"), 
                    each=length(temp_eq)))
    #_Area & Albedo
    area <- data.frame(
      Time=rep((1:length(area_ground)),4), 
      Area_fraction = c(area_ground, area_white, area_black, alb_planet), 
      Section=rep(c("no daisy", "white", "black", "albedo"), 
                    each=length(area_ground)))

    #_Solar
    sol <- data.frame(
      Time=rep((1:length(solar_c)),1), 
      Solar_constant = c(solar_c), 
      Type=rep(c("solar const."), 
                    each=length(solar_c)))

    #_TvsSF
    TvsSF <- data.frame(
      Solar_factor = c(solar_fct), 
      Temp = c(temp_eq, temp_eqn), 
      Type=rep(c("daisy_world", "no_daisy_world"), 
                    each=length(temp_eq)))
      
    list(area=area, temp=temp, sol=sol, TvsSF=TvsSF)
    
    })
  

  # make plots
  
  #_Temp
  output$graph0 <- renderPlot({
    p <- ggplot(mydata()[["temp"]], 
         aes(x=Time, y=Temp, group=World))    
    p <- p + geom_line(aes(colour = World), size=1.5) + 
         ggtitle("Temperature")
    print(p)
  })
  
  #_Area
  output$graph1 <- renderPlot({
    p <- ggplot(mydata()[["area"]], 
         aes(x=Time, y=Area_fraction, group=Section))    
    p <- p + geom_line(aes(colour = Section), size=1.5) + 
         ggtitle("Area fraction & Albedo")
    print(p)
  })
    
  #_Solar
  output$graph2 <- renderPlot({
    p <- ggplot(mydata()[["sol"]], 
         aes(x=Time, y=Solar_constant, group=Type))    
    p <- p + geom_line(aes(colour = Type), size=1.5) + 
         ggtitle("Solar constant")
    print(p)
  })

  #_Temp_vs_Solar_factor
  output$graph3 <- renderPlot({
    p <- ggplot(mydata()[["TvsSF"]], 
         aes(x=Solar_factor, y=Temp, group=Type))    
    p <- p + geom_path(aes(colour = Type), size=1.5) + 
         ggtitle("T vs Solar factor")
    print(p)
  })

  
})
  
