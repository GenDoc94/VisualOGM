# Contribuir a BioCalc

Gracias por tu interés en mejorar BioCalc.

## Cómo reportar un problema

1. Comprueba que no exista ya un [issue abierto](https://github.com/GenDoc94/BioCalc/issues).
2. Abre un issue nuevo con:
   - versión de BioCalc (pestaña **Información**),
   - versión de R (`sessionInfo()`),
   - pasos para reproducir el error,
   - mensaje de error completo (si lo hay),
   - ejemplo anonimizado de nombres de archivo (sin datos de pacientes).

## Cómo proponer cambios

1. Haz fork del repositorio.
2. Crea una rama descriptiva (`fix-oncoprint-labels`, `docs-readme`, etc.).
3. Mantén los cambios acotados a un objetivo.
4. Prueba la app localmente con `shiny::runApp()`.
5. Abre un pull request explicando el problema y la solución.

## Estilo de código

- Sigue el estilo ya usado en `app.R` y `functions/`.
- No incluyas datos clínicos reales en commits, issues ni capturas.

## Privacidad

BioCalc procesa los archivos en el servidor Shiny donde se ejecute la app. Si despliegas una instancia pública, deja claro en el README quién opera el servidor y cómo se tratan los datos.
