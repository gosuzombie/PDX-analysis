options(java.parameters = "-Xmx8000m")

library(openxlsx)
library(RecordLinkage)
curation <- read.csv("drugs_with_ids_new_new.csv")
curation <- curation[, -1]

drugs <- read.xlsx("nm.3954-S2.xlsx", sheet = 4)

curated_drugs <- data.frame(pdx.drugid=NA, unique.drugid=NA)[numeric(0), ]

drugs$Tumor.Type <- gsub("GC", "stomach", drugs$Tumor.Type)
drugs$Tumor.Type <- gsub("BRCA", "breast", drugs$Tumor.Type)
drugs$Tumor.Type <- gsub("CM", "skin", drugs$Tumor.Type)
drugs$Tumor.Type <- gsub("CRC", "large_intestine", drugs$Tumor.Type)
drugs$Tumor.Type <- gsub("NSCLC", "lung", drugs$Tumor.Type)
drugs$Tumor.Type <- gsub("PDAC", "pancreas", drugs$Tumor.Type)

for(d in unique(drugs$Treatment))
{
  
  d2 <- data.frame(strsplit(d, " + ", fixed = TRUE))[,1]
  for(ds in 1:length(d2))
  {
    unique_drug <- NA
    found <- FALSE
    if(d2[ds][[1]] %in% curated_drugs$pdx.drugid)
    {
      message("found previous entry")
      d2[ds][[1]] <- curated_drugs[grep(ds, curated_drugs$pdx.drugid), "unique.drugid"]
      found <- TRUE
    }
  }
  
  if(!found)
  {
    message("not found")
    seen <- c()
    for(x in 1:nrow(curation))
    {
      for(y in 1:ncol(curation))
      {
        #message(paste(x,y, sep = " "))
        if(!is.na(curation[x,y]) && !(curation[x,y] %in% seen))
        {
          for(ds in 1:length(d2))
          {
            if(levenshteinSim(d2[ds][[1]], curation[x,y]) == 1)
            {
              messge("found exact match")
              d2[ds][[1]] <- curation[x,"unique.drugid"]
              curated_drugs <- rbind(curated_drugs, data.frame(pdx.drugid=d2[ds][[1]], unique.drugid=curation[x,"unique.drugid"])[1, ])
              found <- TRUE
            }
            else if(levenshteinSim(d2[ds][[1]], curation[x,y]) >= 0.4)
            {
              message(paste(d2[ds][[1]], curation[x,y], sep = " "))
              z <- readline(prompt = "?????: ")
              if(z == "y")
              {
                d2[ds][[1]] <- curation[x,"unique.drugid"]
                curated_drugs <- rbind(curated_drugs, data.frame(pdx.drugid=d2[ds][[1]], unique.drugid=curation[x,"unique.drugid"])[1, ])
                found <- TRUE
              }
            }
          }
        }
        seen <- c(seen, curation[x,y])
      }
    }
    
    if(!found)
    {
      curated_drugs <- rbind(curated_drugs, data.frame(pdx.drugid=d2[ds][[1]], unique.drugid=d2[ds][[1]]))
    }
  }
  drugs$Treatment <- gsub(d, paste(d2, collapse = " + "), drugs$Treatment)
  message("next drug")
}

