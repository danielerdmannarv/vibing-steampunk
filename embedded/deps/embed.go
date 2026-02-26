	package deps

	// Placeholder for dependency management
	// Note: abapgit-standalone.zip and abapgit-dev.zip are not embedded in this version

// DependencyInfo describes an available dependency
type DependencyInfo struct {
	Name        string // e.g., "abapgit-standalone", "abapgit-dev"
	Description string // Human-readable description
	Package     string // Default SAP package (e.g., "$ABAPGIT")
	Available   bool   // Whether the ZIP is embedded
}

// DeploymentObject represents a single ABAP object to deploy
type DeploymentObject struct {
	Type     string // Object type (PROG, CLAS, INTF, etc.)
	Name     string // Object name
	FileName string // Source file name
	Content  string // File content or path
}

// DeploymentPlan describes objects to deploy from a source
type DeploymentPlan struct {
	Source       string
	Package      string
	Objects      []DeploymentObject
	TotalObjects int
	TotalFiles   int
}

// FileEntry represents a file extracted from a ZIP
type FileEntry struct {
	Name    string
	Content []byte
}

// GetDependencyZIP returns the byte slice for a given source name
func GetDependencyZIP(name string) []byte {
	// Dependencies are fetched at runtime or via external sources
	return nil
}

// GetAvailableDependencies returns the list of available dependencies
func GetAvailableDependencies() []DependencyInfo {
	return []DependencyInfo{
		{
			Name:        "abapgit-standalone",
			Description: "abapGit standalone program (single ZABAPGIT program)",
			Package:     "$ABAPGIT",
			Available:   false, // Not embedded - requires external source
		},
		{
			Name:        "abapgit-dev",
			Description: "abapGit developer edition (full $ZGIT_DEV packages)",
			Package:     "$ZGIT_DEV",
			Available:   false, // Not embedded - requires external source
		},
	}
}

// UnzipInMemory extracts files from a ZIP byte slice without touching disk
// Returns a slice of FileEntry with file names and contents
func UnzipInMemory(zipData []byte) ([]FileEntry, error) {
	// Placeholder implementation - actual implementation would parse ZIP
	// For now, return empty slice since dependencies are not embedded
	return []FileEntry{}, nil
}

// CreateDeploymentPlan creates a deployment plan from extracted files
func CreateDeploymentPlan(source, packageName string, files []FileEntry) *DeploymentPlan {
	plan := &DeploymentPlan{
		Source:  source,
		Package: packageName,
		Objects: []DeploymentObject{},
	}

	// Placeholder: would parse .xml files and create deployment objects
	plan.TotalFiles = len(files)
	plan.TotalObjects = 0

	return plan
}
