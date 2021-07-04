
Fred_API_Keys <- toString(as.character(read.table("~/Fred_API_Keys.txt", quote="\"", comment.char="")[1,1]))



# You can find this also here - https://docs.google.com/spreadsheets/d/1jnqjM0Bb58vrFtxj2E9JCNKYm74d3LigeAMlqrJAEJw/edit#gid=798621230
Data_Series <- read.csv("Data Collection - Fred.csv")


######## Method 1 Using the Fred API #######
library(fredr)
fredr_set_key(Fred_API_Keys )
test <- fredr(
  series_id = "SIPOVGINIARM"
)

######## Method 2 Using Quandl pull the data from Fred Indirectly ####
library(Quandl)
rGDP <- Quandl("FRED/SIPOVGINIARM", type="zoo")




# 
# 
# library(googleCloudStorageR)
# library(bigQueryR)
# 
# gcs_global_bucket("studious-matrix-306102")
# 
# ## custom upload function to ignore quotes and column headers
# f <- function(input, output) {
#   write.table(input, sep = ",", col.names = TRUE, row.names = FALSE, 
#               quote = FALSE, file = output, qmethod = "double")}
# 
# ## upload files to Google Cloud Storage
# gcs_upload(mtcars, name = "mtcars_test1.csv", object_function = f)
# gcs_upload(mtcars, name = "mtcars_test2.csv", object_function = f)
# 
# ## create the schema of the files you just uploaded
# user_schema <- schema_fields(mtcars)
# 
# ## load files from Google Cloud Storage into BigQuery
# bqr_upload_data(projectId = "raw_data", 
#                 datasetId = "test", 
#                 tableId = "from_gcs_mtcars", 
#                 upload_data = c("gs://your-project/mtcars_test1.csv", 
#                                 "gs://your-project/mtcars_test2.csv"),
#                 schema = user_schema)