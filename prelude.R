# helper function to generate the website
bizard_oxford_and <- function(x, code = TRUE, quote = TRUE, sep = ", ") {
    bizard_oxford_comma(
        bizard_code_quote(x, code, quote),
        sep = sep, final = "and"
    )
}

bizard_oxford_or <- function(x, code = TRUE, quote = TRUE, sep = ", ") {
    bizard_oxford_comma(
        bizard_code_quote(x, code, quote),
        sep = sep, final = "or"
    )
}

bizard_code_quote <- function(x, code = TRUE, quote = TRUE) {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    x
}

bizard_oxford_comma <- function(x, sep = ", ", final = "and") {
    n <- length(x)

    if (n < 2L) return(x) # styler: off

    head <- x[seq_len(n - 1L)]
    last <- x[n]

    head <- paste(head, collapse = sep)

    # Write a or b. But a, b, or c.
    if (n > 2L) {
        paste0(head, sep, final, " ", last)
    } else {
        paste0(head, " ", final, " ", last)
    }
}

bizard_setup <- function(..., lang = NULL, title = NULL, system = NULL) {
    lang <- match.arg(lang, c("en", "zh"))

    # Collect unique package names passed in through "..."
    pkgs <- unique(c(...))

    # Get DESCRIPTION metadata for each package
    desc <- lapply(pkgs, function(pkg) utils::packageDescription(pkg))

    # ---- Detect if any package is from Bioconductor ----
    from_bioc <- vapply(desc, function(d) !is.null(d$biocViews), logical(1L))
    if (any(from_bioc)) {
        # Ensure BiocManager itself is included
        pkgs <- unique(c("BiocManager", pkgs))
    }
    from_github <- vapply(desc, function(d) !is.null(d$GithubRepo), logical(1L))
    if (any(from_github)) {
        pkgs <- unique(c("remotes", pkgs))
    }

    # ---- Setup metadata for documentation ----
    title <- title %||%
        sprintf(
            "## %s",
            switch(lang,
                en = "Setup",
                zh = "环境配置"
            )
        )

    language <- "R"
    system <- system %||% sprintf(
        "## %s",
        switch(lang,
            en = "Cross-platform (Linux/MacOS/Windows)",
            zh = "跨平台（Linux/MacOS/Windows）"
        )
    )

    # ---- Generate code snippets for installation & loading ----
    codes <- c(
        "# install packages",
        unlist(lapply(pkgs, function(pkg) {
            description <- utils::packageDescription(pkg)
            c(
                sprintf('if (!requireNamespace("%s", quietly = TRUE)) {', pkg),
                if (pkg != "remotes" &&
                    !is.null(username <- description$GithubUsername) &&
                    !is.null(repo <- description$GithubRepo)) {
                    sprintf(
                        '  remotes::install_github("%s/%s")',
                        username, repo
                    )
                } else if (pkg != "BiocManager" &&
                    !is.null(description$biocViews)) {
                    sprintf('  BiocManager::install("%s")', pkg)
                } else {
                    sprintf('  install.packages("%s")', pkg)
                },
                "}",
                "" # add empty space
            )
        }), FALSE, FALSE),
        "# load packages",
        vapply(
            pkgs, function(pkg) sprintf('library("%s")', pkg),
            character(1L),
            USE.NAMES = FALSE
        )
    )
    docs <- c(
        title,
        "",
        sprintf(
            "- %s: %s",
            switch(lang,
                en = "System Requirements",
                zh = "系统要求"
            ),
            system
        ),
        "",
        sprintf(
            "- %s: %s",
            switch(lang,
                en = "Programming language",
                zh = "编程语言"
            ),
            bizard_oxford_or(
                paste0("**", language, "**"),
                code = FALSE, quote = FALSE
            )
        ),
        "",
        sprintf(
            "- %s: %s",
            switch(lang,
                en = "Dependent packages",
                zh = "依赖包"
            ),
            bizard_oxford_and(pkgs, quote = FALSE)
        ),
        "",
        '```{r, collapse = TRUE, class.source = "fold-hide"}',
        codes,
        "```",
        ""
    )
    # ---- Render docs via knitr ----
    cat(knitr::knit_child(text = docs, quiet = TRUE))
}

bizard_datasource <- function(..., dir = NULL, sep = NULL) {
    if (is.null(dir)) dir <- "datasource"
    if (is.null(sep)) sep <- .Platform$file.sep
    root <- fs::path_rel(
        Sys.getenv("QUARTO_DOCUMENT_PATH"),
        Sys.getenv("QUARTO_DOCUMENT_ROOT")
    )
    subdir <- fs::path_ext_remove(Sys.getenv("QUARTO_DOCUMENT_FILE"))
    file.path(root, dir, subdir, ..., fsep = sep)
}

#' Create a Markdown link to a project data repository
#'
#' Constructs a GitHub-style Markdown link pointing to a directory in the
#' repository, using the current Quarto `params` for URL and branch if not
#' provided.
#'
#' @param label Character. The link text to display. Default
#' `"Data Repository"`.
#' @param dir Character. Subdirectory relative to the document directory.
#'   Passed to [`datasource()`]. Default `datasource`.
#' @param url Character. Repository URL. If `NULL`, tries to read from
#' `params[['repo-url']]`.
#' @param branch Character. Repository branch. If `NULL`, tries to read from
#' `params[['repo-branch']]`.
#' @return Character string containing a Markdown link.
bizard_datasource_md_link <- function(label = "Data Repository",
                                      dir = NULL, url = NULL, branch = NULL) {
    if (is.null(url) && (
        !exists("params", envir = globalenv(), mode = "list") ||
            is.null(url <- params[["repo-url"]]) # nolint
    )) {
        url <- "https://github.com/openbiox/Bizard"
    }
    if (is.null(branch) && (
        !exists("params", envir = globalenv(), mode = "list") ||
            is.null(branch <- params[["repo-branch"]]) # nolint
    )) {
        branch <- "main"
    }
    sprintf(
        "[%s](%s)",
        label,
        file.path(
            url,
            "tree",
            branch,
            bizard_datasource(dir = dir, sep = "/"),
            fsep = "/"
        )
    )
}
