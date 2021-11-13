
files <- paste0("./scripts/",list.files("./scripts"))


for (i in 1:9) {
        
        source(files[i], encoding = "UTF-8")
}

rm(files,lista)
