mod.df <- read.Alteryx("#1")
mod.obj <- unserializeObject(as.character(mod.df$Object[1]))
the.class <- class(mod.obj)[1]
write.Alteryx(data.frame(Class = the.class))
