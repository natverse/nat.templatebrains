# nat.templatebrains 0.9

* shortest_bridging_seq complains if template brain does not exist in graph (#34)
* xform_brain does not transform when sample and reference are identical (#33)
* templatebrain constructor does not need all boundingbox/origin etc fields
  to be specified (#24)
* as.templatebrain.im3d should not need an im3d made from a file (#25)
* Fix unstated dependencies in 'tests' error noted by CRAN r-devel for 0.8.2

# nat.templatebrains 0.8

This is another major version bump since there is significant new/changed
behaviour in xform_brain brain and friends which will now tag transformed
objects with a registration template space. This means that nat functions will
typically not be need to told the space of an object such as a neuron list in
many cases, but some objects will have a new regtemplate attribute.

* xform_brain should tag objects with their space (#16)
* add regtemplate function as part of this
* fix xform_brain should give better error when sample unknown (#29)
* fix: teach plot3d.templatebrain to work with regtemplate (#32)
* fix: add_reglist should not return anything (visibly) (#30)

# nat.templatebrains 0.7

This is the first public release since 0.6.2 and there are some significant
changes under the hood, hence the major version bump.

* fix inversion of reglist objects read from disk (so that xform_brain calls
  depending on reglist objects work in both directions).
* add support for memoisation of shortest_bridging_seq so that we do not need to
  rescan registration directories / reload on disk reglist objects.

# nat.templatebrains 0.6.4

* Add regtemplate function and integrate it with xform_brain and mirror_brain.
  It will often no longer be necessary to specify the initial space of objects
  to be transformed - you'll still need to specify your target space though!

# nat.templatebrains 0.6.3

* add support for reglist objects containing one or more arbitrary transforms
  as bridging or mirroring registrations. A function add_reglist makes it easy
  to add new in memory reglist objects to those that can be used for bridging
  / mirroing between template brains (#21).

# nat.templatebrains 0.6.2

* fix issues with git repository cloning on Windows

# nat.templatebrains 0.6.1

* fix bug in mirror_brain for non-zero origin (#14)

# nat.templatebrains 0.6

* add functions to download new registrations from git repositories:
  add_reg_folder, download_reg_repo, local_reg_dir_for_url, update_reg_repos.
* add tests and documentation for new git functionality.
* downloaded registrations are automatically made available on package load.

# nat.templatebrains 0.5

* xform_brain can now find the best available path from one template to another
  automatically.
* this uses new functions bridging_graph, shortest_bridging_seq and
  allreg_dataframe
* xform_brain now has an imagedata argument (to set sensible defaults for
  handling of inverse registrations, which are very slow for image data)
* add doi field to templatebrain object
* fix: mirror_brain passes on transform argument
* dev: now imports igraph
* dev: requires nat >=1.7.0

# nat.templatebrains 0.4.1

* fix handling of regName in templatebrain
* fix as.templatebrain.im3d (completely broken due to variable name error)
* better defaults in as.templatebrain.im3d / as.templatebrain.character
  (both name and short name will be take from file name)
* print.templatebrain now includes short name

# nat.templatebrains 0.4

* prepare for CRAN release
* fix export of is.templatebrain
* give mirror_brain explicit transform argument
* full examples in mirror_brain and xform_brain functions and
  templatebrain-meths.
* add FCWB.demo as data object that can be loaded
* dev: integrate with nat.flybrains build on travis

# nat.templatebrains 0.3

* add bridging_sequence function to return a bridging registrations sequence
  that may span more than 2 template brains.
* xform_brain can apply compound registrations in one streamxform call
  (but depends on nat >=1.5.9 and CMTK >= 3.2.2)
* add is.templatebrain and as.character.templatebrain utility functions.
* switch to single option regdirs for all paths
* no longer suggests nat.flybrains but includes basic tests with internal data.

# nat.templatebrains 0.2

* move all generic functionality from nat.flybrains (which is now a data only
  package).
* give xform_brain a via option
* add options for directories containing bridging and mirroring registrations
  nb these can accept multiple dirs, but registration names must be globally
  unique.
* suggest nat.flybrains for tests
* package docs and tests
