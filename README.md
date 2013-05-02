-- Creates a new version of a template.

> Usage: hgc-version [Option...] template
>   -y             --publish                Automatically publish new version.
>   -M             --major                  Create a major revision.
>   -n NAME        --new-capsule=NAME       New capsule name.
>   -r REPOSITORY  --repository=REPOSITORY  Repository name (defaults to mercury.repo)

- If <newname> not set, increment a version number
- Get repo
- cvmfs_server transaction repo
- Copy newname to oldname
- Copy config file to temporary location
- Modify temporary config file to adjust location of rootfs and fstab
- lxc-start and lxc-console
- Clean (remove cache dirs)
- Publish (possibly - maybe prompt?)