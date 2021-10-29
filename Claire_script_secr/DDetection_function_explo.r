# -----> All models with mask

Mcat02 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)
# D ~ 0.0024/ha (CLOSED), 0.0018/ha (OPEN)(*100 for conversion in km2)
# g0 ~ 0.096
# sigma ~ 848
# z ~ 3.95

Mcat04 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = T)
# D ~ 0.0021/ha = 0.21/km2
# g0 ~ 0.096
# sigma ~ 820m
# z ~ 3.94

Mcat05 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)


Mcat06 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~session),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)
# D ~ 0.0028/ha
# g0 ~ 0.094
# sigma ~ 545 (CLOSED) / 898 (OPEN)
# z ~ 3.55 

Mcat07 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~session),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat08 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~session),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat09 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat10 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat11 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~session),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat12 <- secr.fit(cat_maido,
                   model = list(D~session, g0~session, sigma~session),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat13 <- secr.fit(cat_maido,
                   model = list(D~session, g0~session, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat14 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1),
                   detectfn = 'HN', # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

AIC(Mcat02,Mcat04,Mcat05,Mcat06,Mcat07,Mcat08,Mcat09,Mcat10,Mcat11,Mcat12,Mcat13,Mcat14)