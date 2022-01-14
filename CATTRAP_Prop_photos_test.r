pic.number <- matrix(c(114,
                       262,
                       21542-114,
                       20381-262),
                     nrow = 2,
                     dimnames = list("milieu" = c("trails", "vegetation"),
                                     "cat" = c("yes", "no")))

mcnemar.test(pic.number) # pour échantillons apparaiés
prop.test(x = c(114, 262),
          n = c(21524, 20381),
          alternative = 'two.sided') # pour échantillons indépendants
