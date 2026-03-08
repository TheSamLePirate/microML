### TODO
- ajouter des tests eval et typer (le passing d'options de debug au pipeline)
- généralisation des lets dans le typer
- passe pour transformer les && et || en let (ainsi qu'un test associé)
- une passe d'inline (sur l'ast backend)
- compaction des applications lors du backend
- une passe de suppression des clotures
- garbage collector
- ajouter unit, true, false ?

### pour bootstrap
- ajouter des ou et des constantes dans les patterns
- gérer les modules et modules type
- gérer le coerce
- compilation de fichiers séparés
- une lib standard
   - un module List
   - un module Set
   - un module Map
   - un module Format
   - un module Unix
- supprimer les exceptions, tout écrire avec des options, ou ajouter les exceptions dans le backend du mini-ml
