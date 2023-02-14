
? remove need to traverse the store so we are left with only GET and PUT operations

the store now needs a httpTransport :-/ by the time we use the store we
must have initialised it but it's not routed there.

I should not change doesStoreEntryExist because that's involved in the
race mechanism.

I think the change has to start from getStoreEntries. For each package in
the build plan we change if a remote cache exists, if so download the
remote package into our store (perhaps still using newStoreEntry).

getStoreEntries gives a Rebuild! this is annoying

looking at improveInstallPlanWithInstalledPackages there's this invariant where
installed packages can only depend on installed packages. So cabal should
check the remote cache in topological order, and stop at the first package
missing from the remote cache.
