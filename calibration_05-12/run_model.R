
run_model = function(par, conf, osmose, is_a_test=FALSE, version="4.3.3", options="-Xmx3g -Xms1g", ...) {
  
  nspp = get_species(conf, type="focal", code=TRUE)
  nfsh = get_fisheries(conf, code=TRUE)
  ndtperyear = get_par(conf, 'simulation.time.ndtperyear')
  nyear      = get_par(conf, 'simulation.time.nyear')
  ndt = nyear*ndtperyear
  
  larval_deviates  = get_par(par, 'osmose.user.larval.deviate.log')
  fishing_deviates = get_par(par, 'fisheries.rate.byperiod.log')
  
  for(isp in nspp) {
    nn = sprintf('mortality.additional.larva.rate.seasonality.sp%s', isp)
    ldev = get_par(larval_deviates, sp=as.numeric(isp))
    par[[nn]] = exp(calibrar::spline_par(ldev, n=ndt)$x)  
  }
  
  d75 = get_par(par, "selectivity.delta75.fsh") # all of them
  l50 = get_par(par, "selectivity.l50.fsh") # all of them
  L50 = get_par(conf, "selectivity.l50.fsh")

  for(ifsh in nfsh) {
    nn = sprintf('fisheries.selectivity.l75.fsh%s', ifsh)
    this.d75 = get_par(d75, fsh=as.numeric(ifsh))
    if(is.null(this.d75)) next
    this.l50 = get_par(l50, fsh=as.numeric(ifsh))
    if(is.null(this.l50)) this.l50 = get_par(L50, fsh=as.numeric(ifsh))
    par[[nn]] = this.l50 + this.d75    
  }  
  
  ### BEGIN USER ADDED

  # I catchability 

  # 1. load all the parameters related to the catchability matrix
  q_fsh0 = get_par(conf=par, par="osmose.user.catchability.fsh0")
  q_fsh0_sp0 = get_par(q_fsh0, sp=0)
  q_fsh0_sp1 = get_par(q_fsh0, sp=1)
  q_fsh0_sp2 = get_par(q_fsh0, sp=2)
  q_fsh0_sp3 = get_par(q_fsh0, sp=3)
  q_fsh0_sp5 = get_par(q_fsh0, sp=5)
  q_fsh0_sp7 = get_par(q_fsh0, sp=7)
  q_fsh0_sp8 = get_par(q_fsh0, sp=8)
  q_fsh0_sp9 = get_par(q_fsh0, sp=9)
  q_fsh0_sp10 = get_par(q_fsh0, sp=10)
  q_fsh0_sp13 = get_par(q_fsh0, sp=13)
  q_fsh0_sp14 = get_par(q_fsh0, sp=14)
  q_fsh0_sp15 = get_par(q_fsh0, sp=15)
  
  q_fsh1 = get_par(conf=par, par="osmose.user.catchability.fsh1")
  q_fsh1_sp9 = get_par(q_fsh1, sp=9)
  q_fsh1_sp10 = get_par(q_fsh1, sp=10)
  q_fsh1_sp11 = get_par(q_fsh1, sp=11)
  q_fsh1_sp12 = get_par(q_fsh1, sp=12)
  
  q_fsh2 = get_par(conf=par, par="osmose.user.catchability.fsh2")
  q_fsh2_sp0 = get_par(q_fsh2, sp=0)
  q_fsh2_sp5 = get_par(q_fsh2, sp=5)
  q_fsh2_sp7 = get_par(q_fsh2, sp=7)
  q_fsh2_sp8 = get_par(q_fsh2, sp=8)
  
  q_fsh3 = get_par(conf=par, par="osmose.user.catchability.fsh3")
  q_fsh3_sp0 = get_par(q_fsh3, sp=0)
  q_fsh3_sp1 = get_par(q_fsh3, sp=1)
  q_fsh3_sp2 = get_par(q_fsh3, sp=2)
  q_fsh3_sp3 = get_par(q_fsh3, sp=3)
  q_fsh3_sp5 = get_par(q_fsh3, sp=5)
  q_fsh3_sp7 = get_par(q_fsh3, sp=7)
  q_fsh3_sp8 = get_par(q_fsh3, sp=8)
  q_fsh3_sp9 = get_par(q_fsh3, sp=9)
  q_fsh3_sp10 = get_par(q_fsh3, sp=10)
  q_fsh3_sp11 = get_par(q_fsh3, sp=11)
  q_fsh3_sp12 = get_par(q_fsh3, sp=12)
  q_fsh3_sp13 = get_par(q_fsh3, sp=13)
  q_fsh3_sp14 = get_par(q_fsh3, sp=14)
  q_fsh3_sp15 = get_par(q_fsh3, sp=15)

  # 2. read the catchability matrix
  catchability.matrix = read.csv(file="eec_fisheries_catchability_template.csv", check.names = FALSE, row.names = 1)
  # 3. modify the catchability matrix
  catchability.matrix[1,1] = q_fsh0_sp0
  catchability.matrix[2,1] = q_fsh0_sp1
  catchability.matrix[3,1] = q_fsh0_sp2
  catchability.matrix[4,1] = q_fsh0_sp3
  catchability.matrix[6,1] = q_fsh0_sp5
  catchability.matrix[8,1] = q_fsh0_sp7
  catchability.matrix[9,1] = q_fsh0_sp8
  catchability.matrix[10,1] = q_fsh0_sp9
  catchability.matrix[11,1] = q_fsh0_sp10
  catchability.matrix[14,1] = q_fsh0_sp13
  catchability.matrix[15,1] = q_fsh0_sp14
  catchability.matrix[16,1] = q_fsh0_sp15

  catchability.matrix[10,2] = q_fsh1_sp9
  catchability.matrix[11,2] = q_fsh1_sp10
  catchability.matrix[12,2] = q_fsh1_sp11
  catchability.matrix[13,2] = q_fsh1_sp12

  catchability.matrix[1,3] = q_fsh2_sp0
  catchability.matrix[6,3] = q_fsh2_sp5
  catchability.matrix[8,3] = q_fsh2_sp7
  catchability.matrix[9,3] = q_fsh2_sp8

  catchability.matrix[1,4] = q_fsh3_sp0
  catchability.matrix[2,4] = q_fsh3_sp1
  catchability.matrix[3,4] = q_fsh3_sp2
  catchability.matrix[4,4] = q_fsh3_sp3
  catchability.matrix[6,4] = q_fsh3_sp5
  catchability.matrix[8,4] = q_fsh3_sp7
  catchability.matrix[9,4] = q_fsh3_sp8
  catchability.matrix[10,4] = q_fsh3_sp9
  catchability.matrix[11,4] = q_fsh3_sp10
  catchability.matrix[12,4] = q_fsh3_sp11
  catchability.matrix[13,4] = q_fsh3_sp12
  catchability.matrix[14,4] = q_fsh3_sp13
  catchability.matrix[15,4] = q_fsh3_sp14
  catchability.matrix[16,4] = q_fsh3_sp15

  # 4. write the new modified catchability matrix
  write.csv(catchability.matrix, file="eec_fisheries_catchability.csv")
  #write_osmose(catchability.matrix, file="eec_fisheries_catchability_wo.csv", col.names = TRUE) # problem with row names 2023-12-01

  # II additional mortality multiplier 

  # 1. load parameters
  multiplier_m_add_sp7 = get_par(conf=par, par="osmose.user.multiplier.additional.mortality.sp7")
  multiplier_m_add_sp8 = get_par(conf=par, par="osmose.user.multiplier.additional.mortality.sp8")

  # 2. read mortality vector
  m_add_sp7 = read.csv("additional_mortality-sole_template.csv", row.names = 1)
  m_add_sp8 = read.csv("additional_mortality-plaice_template.csv", row.names = 1)
  
  # 3. calculate additional mortality by time step
  # calculate average annual mortality
  m_add_annual_mean_sp7 = sum(m_add_sp7)/20
  m_add_annual_mean_sp8 = sum(m_add_sp8)/20

  # calculate annual mortality vector
  m_add_annual_sp7 = m_add_annual_mean_sp7 * multiplier_m_add_sp7
  m_add_annual_sp8 = m_add_annual_mean_sp8 * multiplier_m_add_sp8

  # calculate mortality by time step
  year = seq(1,20)
  time_step_per_year = seq(1,20,length.out=480)
  m_add_time_step_sp7 = approx(year, m_add_annual_sp7 , xout=time_step_per_year)
  m_add_time_step_sp7 = m_add_time_step_sp7$y/24
  m_add_time_step_sp8 = approx(year, m_add_annual_sp8 , xout=time_step_per_year)
  m_add_time_step_sp8 = m_add_time_step_sp8$y/24

  # 4. write the new modified mortality vector
  write.csv(m_add_time_step_sp7, file="additional_mortality-sole.csv")
  write.csv(m_add_time_step_sp8, file="additional_mortality-plaice.csv")

### END USER ADDED
  
  # remove all osmose.user parameters and clean-up
  par = get_par(par, "osmose.user", invert = TRUE, as.is=TRUE)
  par = get_par(par, linear=TRUE, as.is = TRUE) # write parameters in linear scale.

  write_osmose(par, file='calibration_parameters.osm')
  # run osmose!
  if(!isTRUE(is_a_test)) {
    run_osmose(input='osmose-calibration.osm', output='output', osmose=osmose, 
               version = version, options=options, verbose=FALSE)
  }
  
  output = read_osmose(path='output', version=version, null.on.error=TRUE)
  
  names(larval_deviates)  = paste("larval", get_species(conf, nm=names(larval_deviates)), sep="_")
  names(fishing_deviates) = paste("F", get_fisheries(conf, nm=names(fishing_deviates)), sep="_")
  
  # random = list(larval  = sapply(larval_deviates,  FUN=function(x) sum(x^2)),
  #               fishing = sapply(fishing_deviates, FUN=function(x) sum(x^2)))
  
  cal_output = c(biomass = get_var(output, "biomass", how="list", no.error = TRUE),
                 yield   = get_var(output, "yieldBySpecies", how="list", no.error = TRUE),
                 catchatlength = get_var(output, "yieldNBySize", how="list", no.error = TRUE),
                 mortality = get_var(output, "residualSizeByAge", how="list", no.error = TRUE),
                 growth = get_var(output, "residualMortalityByAge", how="list", no.error = TRUE)
                 )
  
  if(is.null(cal_output)) {
    # sometimes, you get a 'no-write' error. Restarting the calibration usually fix the problem.
    # several things can be the issue. Most times, you won't see the error again. 
    # Those are caused by the ghosts in the machine (DATARMOR). To please them, we will run the 
    # model again, and see.
    if(!isTRUE(is_a_test)) {
      run_osmose(input='osmose-calibration.osm', output='output', osmose=osmose, 
                 version = version, options=options, verbose=FALSE)
    }
    
    output = read_osmose(path='output', version=version, null.on.error=TRUE)
    
    cal_output = c(biomass = get_var(output, "biomass", how="list", no.error = TRUE),
                   yield   = get_var(output, "yieldBySpecies", how="list", no.error = TRUE),
                   catchatlength = get_var(output, "yieldNBySize", how="list", no.error = TRUE),
                   mortality = get_var(output, "residualSizeByAge", how="list", no.error = TRUE),
                   growth = get_var(output, "residualMortalityByAge", how="list", no.error = TRUE)
                   )
    
  }
  
  # if is still NULL, we will let calibrar to deal with it.
  if(is.null(cal_output)) {
    cal_output = NULL
    message("Something wrong happened while running 'run_model'. Returning NULL. Check the '.calibrar_dump' folder.")
  }

  cal_output = c(cal_output, random=larval_deviates, random=fishing_deviates)
  
  return(invisible(cal_output))
  
}

