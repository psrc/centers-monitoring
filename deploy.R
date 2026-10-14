library(rsconnect)

# official version ----

deployApp(account = 'psrcwa',
         appName = 'centers-monitoring',
         appTitle = 'PSRC Centers Dashboard')

# draft version ----

deployApp(account = 'psrcwa',
          appName = 'draft-centers-monitoring',
          appTitle = 'DRAFT PSRC Centers Dashboard')
