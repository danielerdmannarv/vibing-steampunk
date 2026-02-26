package deps

// Placeholder for dependency management
// Note: abapgit-standalone.zip and abapgit-dev.zip are not embedded in this version

// GetDependencyZIP returns the byte slice for a given source name
func GetDependencyZIP(name string) []byte {
	// Dependencies are fetched at runtime or via external sources
	return nil
}

// Update GetAvailableDependencies to reflect that they are now available
func GetAvailableDependencies() []DependencyInfo {
	return []DependencyInfo{
		{
			Name:        "abapgit-standalone",
			Description: "abapGit standalone program",
			Package:     "$ABAPGIT",
			Available:   false, // Not embedded - requires external source
		},
		{
			Name:        "abapgit-dev",
			Description: "abapGit developer edition",
			Package:     "$ZGIT_DEV",
			Available:   false, // Not embedded - requires external source
		},
	}
}
