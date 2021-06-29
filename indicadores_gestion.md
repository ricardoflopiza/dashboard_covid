---
title: "Indicadores de gestión"
output: html_document:
        theme: null
        highlight: null
        mathjax: null
---
# Indicadores de gestión

Los datos están construidos sobre el total de vivienda gestionadas. Dentro de cada vivienda se selecciona el hogar con el estado más avanzado, lo que implica que **no** están siendo contabilizados los **segundos hogares**.  

## Definiciones

**unidades iniciadas**: unidades con alguna gestión de parte del encuestador 

**unidades completadas**: unidades con gestión terminada por el EG o el encuestador (*interview_status* == 100 | *interview_status* == 120 )

**unidades aprobadas**: unidades aprobadas por el encargado de grupo (*interview_status* == 120 | *interview_status* == 130)

**unidades rechazadas EG**: unidades rechazadas por el encargado de grupo (*interview_status* == 65)

**pendientes encuestador**: asignadas - completadas

**pendientes EG**: completadas - aprobadas_eg

**por_iniciadas**: $\frac{iniciadas}{tamaño \ muestra} * 100$

**por_completadas**: $\frac{completadas}{tamaño \ muestra}*100$

**por_aprobadas**: $\frac{aprobadas}{tamaño \ muestra}*100$

**por_pendiente_enc **: $\frac{pendientesEncuestador }{asignadas }*100$

**por_pendiente_eg **: $\frac{pendientesEncuestador }{completadas}*100$
