---
synopsis: "Report configure failures as \"Failed to configure\""
packages: [cabal-install]
significance: normal
---

A package that failed during the configure step was reported as
"Failed to build ...", which was misleading. Such failures are now
reported as "Failed to configure ...".
