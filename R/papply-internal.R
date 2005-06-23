".Last" <-
function(){
    if (is.loaded("mpi_initialize")){
        if (mpi.comm.size(1) > 0){
            #print("Please use mpi.close.Rslaves() to close slaves.")
            mpi.close.Rslaves()
        }
        #print("Please use mpi.quit() to quit R")
        mpi.finalize()
    }
}

