-- Creates a new version of a template.

	Create a new version of a Mercury capsule based on the specified template.
	Usage: hgc-version [Option...] template
	                  --amend                  Amend the current capsule rather than creating a new one.
	                  --clone-only             Only clone the template, don't start the capsule.
	  -m MOUNT_POINT  --mount=MOUNT_POINT      Mount the specified resource into the capsule.
	  -M              --major                  Create a major revision.
	  -n NAME         --new-capsule=NAME       Create a capsule under a new name.
	  -p PKGDIR       --pkgdir=PKGDIR          Use specified directory in place of the aura source package cache.
	  -r REPOSITORY   --repository=REPOSITORY  Use the specified repository name (defaults to mercury.repo).
	  -v              --verbose                Enable verbose output.
	  -y              --publish                Automatically publish new capsule.

- If <newname> not set, increment a version number
- Get repo
- cvmfs_server transaction repo
- Copy newname to oldname
- Copy config file to temporary location
- Modify temporary config file to adjust location of rootfs and fstab
- lxc-start and lxc-console
- Clean (remove cache dirs)
- Publish (possibly - maybe prompt?)