package deps

import (
	_ "embed" // Required for embedding
	// ... keep your other imports ...
)

// Add these to embed the files
//go:embed abapgit-standalone.zip
var abapGitStandalone []byte

//go:embed abapgit-dev.zip
var abapGitDev []byte

// GetDependencyZIP returns the byte slice for a given source name
func GetDependencyZIP(name string) []byte {
	switch name {
	case "abapgit-standalone":
		return abapGitStandalone
	case "abapgit-dev":
		return abapGitDev
	default:
		return nil
	}
}

// Update GetAvailableDependencies to reflect that they are now available
func GetAvailableDependencies() []DependencyInfo {
	return []DependencyInfo{
		{
			Name:        "abapgit-standalone",
			Description: "abapGit standalone program",
			Package:     "$ABAPGIT",
			Available:   true, // Now true because we embedded it
		},
		{
			Name:        "abapgit-dev",
			Description: "abapGit developer edition",
			Package:     "$ZGIT_DEV",
			Available:   true, // Now true because we embedded it
		},
	}
}
