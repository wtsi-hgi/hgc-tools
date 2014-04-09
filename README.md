Set of tools for working with Mercury capsules.



-- hgc-version: Creates a new version of a template.

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

-- hgc-deploy: Run a capsule based on an exising template (intended to be installed suid root). 

		Launch a Mercury capsule.
		Usage: hgc-deploy [Option...] capsule
		  -m RESOURCE    --mount=RESOURCE         Load the specified resource into the capsule.
		  -r REPOSITORY  --repository=REPOSITORY  Use the specified repository name (defaults to mercury.repo).
		  -t UNION_TYPE  --union-type=UNION_TYPE  Set the type of filesystem used to implement the union mount. Currently supported are aufs and overlayfs.
		  -v             --verbose                Enable verbose output.
		                 --retain-temp            Retain the temporary files under /tmp/hgc

The usage of a range of years within a copyright statement contained within this distribution should be interpreted as being equivalent to a list of years including the first and last year specified and all consecutive years between them. For example, a copyright statement that reads 'Copyright (c) 2005, 2007- 2009, 2011-2012' should be interpreted as being identical to a statement that reads 'Copyright (c) 2005, 2007, 2008, 2009, 2011, 2012' and a copyright statement that reads "Copyright (c) 2005-2012' should be interpreted as being identical to a statement that reads 'Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012'."
