library(lmtest) 

data(ChickEgg)
grangertest(egg ~ chicken, order = 5, data = ChickEgg)
grangertest(chicken ~ egg, order = 5, data = ChickEgg)