# scan_qmd_packages: find used R packages in .qmd files without
scan_qmd_packages <- function(root = ".") {
    # pak::pkg_install()
    # BiocManager::install() // install is a common pattern,
    # remotes::install_github()
    # devtools::install_gitlab()
    # "\\b([a-zA-z][a-zA-z0-9]*])::"
    patterns <- c(
        "(?:install\\.packages|pkg_install|BiocManager::install|install_github|install_gitlab)\\((?:'|\")([a-zA-z].*)(?:'|\")\\)"
    )
    paths <- list.files(".",
        pattern = "\\.qmd$", recursive = TRUE,
        ignore.case = TRUE
    )
    lines <- lapply(paths, function(path) readLines(path))
    lines <- unlist(lines)
    deps <- lapply(patterns, function(pattern) {
        pkgs <- stringr::str_match_all(lines, pattern)
        unlist(
            lapply(pkgs, function(mat) mat[, 2L, drop = TRUE]),
            use.names = FALSE
        )
    })
    unique(unlist(deps, use.names = FALSE))
}

used <- scan_qmd_packages()

current <- desc::desc_get_deps()$package
current <- c(
    current,
    tools::package_dependencies(current, recursive = TRUE)
)
current <- c(
    "rmarkdown", "pak", "remotes", "devtools",
    "BiocManager", unlist(current)
)

# get the package name
deps <- used
remote <- grepl("/", deps)
local_deps <- deps[!remote]
local_deps <- setdiff(local_deps, current)

remote_name <- deps[remote]
remote_deps <- sub("^.*/", "", remote_name)
remote_valid <- !remote_deps %in% c(local_deps, current)
remote_name <- remote_name[remote_valid]
remote_deps <- remote_deps[remote_valid]

new_deps <- c(local_deps, remote_deps)
if (length(new_deps)) {
    desc::desc_set_deps(
        rbind(
            desc::desc_get_deps(),
            data.frame(type = "Imports", package = new_deps, version = "*")
        )
    )
}
if (length(remote_name)) {
    desc::desc_set_remotes(c(desc::desc_get_remotes(), remote_name))
}
