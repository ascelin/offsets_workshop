

ct <- data.frame(

    'TG.Low'  = c(0,1,2),
    'TG.Med1' = c(1,2,4),
    'TG.Med2' = c(1,3,4),
    'TG.High' = c(3,5,6),

    'GG.Low'  = c(2,3,6),
    'GG.Med1' = c(7,9,12),
    'GG.Med2' = c(3,4,8),
    'GG.High' = c(8,12,20),

    'FG.Low'  = c(2,5,10),
    'FG.Med1' = c(8,11,15),
    'FG.Med2' = c(3,5,9),
    'FG.High' = c(12,15,25),

    'TG.cover.Low'  = c(0,2,5),
    'TG.cover.Med1' = c(0,3,5),
    'TG.cover.Med2' = c(10,40,65),
    'TG.cover.High' = c(35,52,70),

    'GG.cover.Low'  = c(5,10,15),
    'GG.cover.Med1' = c(15,40,65),
    'GG.cover.Med2' = c(5,10,15),
    'GG.cover.High' = c(25,40,60)

    ) 

row.names(ct) <- c('lower.bound', 'initial.val', 'upper.bound')