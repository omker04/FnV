source('setup_packages.R')

train_data_fname <- 'lightbulbs_train.csv'
test_data_fname <- 'lightbulbs_test.csv'
attributeData_fname <- 'Lightbulbs ATTRIBUTES 082916_final.csv'

store_nbr
primary_attributes
noSubsAttribute

number_of_simulations
ncores

source('optimization_run.R')
source('ui.R')
source('server.R')
shinyApp(ui, server)


existingWorkspaceName 
get(load(existingWorkspaceName, sys.frame()))
source('ui.R')
source('server.R')
shinyApp(ui, server)