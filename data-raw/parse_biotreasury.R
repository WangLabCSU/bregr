# Parse biotreasury.cn toolList to extract tool IDs
# 
# This script demonstrates how to extract tool IDs from the biotreasury.cn website
# For example, FaceDig tool has ID 214144 (https://biotreasury.cn/tool?id=214144)
#
# Author: Generated for investigating biotreasury.cn tool ID extraction
# Date: 2025-12-19

#' Parse biotreasury.cn toolList page to extract tool IDs
#' 
#' The biotreasury.cn website is a React single-page application (SPA) that
#' loads tool data dynamically via JavaScript. To extract tool IDs, we need to:
#' 
#' 1. Use a headless browser (like RSelenium or chromote) to render the page
#' 2. Wait for the JavaScript to load and populate the tool cards
#' 3. Extract the tool data from the rendered page
#' 
#' Alternative approach: Intercept the API calls that fetch tool data
#' 
#' @section Website Structure:
#' The biotreasury.cn/toolList page loads tool data from an API endpoint.
#' Each tool card contains:
#' - Tool name (e.g., "FaceDig")
#' - Tool ID (e.g., 214144)
#' - Tool description
#' - Other metadata
#' 
#' @section Method 1: Using chromote to render JavaScript
#' chromote is a modern R package for controlling headless Chrome
#' 
#' @section Method 2: Finding and using the API endpoint
#' The React app likely calls an API endpoint to fetch tool data.
#' By inspecting network requests, we can identify and use this endpoint directly.
#' 
#' @examples
#' \dontrun{
#' # Method 1: Using chromote (requires Chrome/Chromium installed)
#' library(chromote)
#' 
#' # Create a browser instance
#' b <- ChromoteSession$new()
#' 
#' # Navigate to the toolList page
#' b$Page$navigate("https://biotreasury.cn/toolList")
#' 
#' # Wait for page to load
#' Sys.sleep(5)
#' 
#' # Extract tool data from the page
#' # The actual selector depends on the page structure
#' tools_html <- b$Runtime$evaluate("document.body.innerHTML")
#' 
#' # Parse the HTML to extract tool IDs
#' # Tool cards likely have links like: /tool?id=214144
#' 
#' # Method 2: Finding the API endpoint
#' # 1. Open browser DevTools (F12)
#' # 2. Go to Network tab
#' # 3. Visit https://biotreasury.cn/toolList
#' # 4. Look for XHR/Fetch requests
#' # 5. Find the request that returns tool data (likely contains tool IDs)
#' # 6. Use that API endpoint directly with httr or curl
#' 
#' # Example API call (endpoint needs to be discovered):
#' library(httr)
#' library(jsonlite)
#' 
#' # Hypothetical API endpoint (actual endpoint may differ)
#' # response <- GET("https://biotreasury.cn/api/tools/list")
#' # tools_data <- fromJSON(content(response, "text"))
#' }

# Required packages for web scraping
required_packages <- c(
  "chromote",    # For headless browser automation
  "rvest",       # For HTML parsing
  "httr",        # For HTTP requests
  "jsonlite",    # For JSON parsing
  "xml2"         # For XML/HTML parsing
)

#' Check and install required packages
#' 
#' @param packages Character vector of package names
check_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Package '", pkg, "' is not installed.")
      message("Install with: install.packages('", pkg, "')")
    }
  }
}

#' Extract tool IDs from biotreasury.cn using chromote
#' 
#' This function uses a headless browser to render the JavaScript-heavy
#' biotreasury.cn website and extract tool IDs from the rendered page.
#' 
#' @param url The URL to parse (default: "https://biotreasury.cn/toolList")
#' @param wait_time Time to wait for page to load (in seconds)
#' @return A data frame with tool information including IDs
#' 
#' @export
#' @examples
#' \dontrun{
#' tools <- extract_biotreasury_tools()
#' # Find FaceDig tool
#' facedig <- tools[tools$name == "FaceDig", ]
#' print(facedig$id)  # Should be 214144
#' }
extract_biotreasury_tools <- function(url = "https://biotreasury.cn/toolList", 
                                      wait_time = 5) {
  
  # Check if chromote is available
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("Package 'chromote' is required. Install with: install.packages('chromote')")
  }
  
  message("Starting headless browser...")
  b <- chromote::ChromoteSession$new()
  
  tryCatch({
    # Navigate to the page
    message("Navigating to ", url)
    b$Page$navigate(url)
    b$Page$loadEventFired()
    
    # Wait for JavaScript to render
    message("Waiting ", wait_time, " seconds for page to load...")
    Sys.sleep(wait_time)
    
    # Method 1: Try to extract links with tool IDs
    # Look for links matching pattern: /tool?id=XXXXXX
    message("Extracting tool links...")
    links_js <- '
      Array.from(document.querySelectorAll("a[href*=\'/tool?id=\']"))
        .map(a => ({
          href: a.href,
          id: new URLSearchParams(a.href.split("?")[1]).get("id"),
          text: a.textContent.trim()
        }))
    '
    
    result <- b$Runtime$evaluate(links_js)
    
    if (!is.null(result$result$value)) {
      tools_data <- result$result$value
      
      # Convert to data frame
      tools_df <- do.call(rbind, lapply(tools_data, function(x) {
        data.frame(
          id = as.character(x$id),
          text = as.character(x$text),
          href = as.character(x$href),
          stringsAsFactors = FALSE
        )
      }))
      
      message("Found ", nrow(tools_df), " tools with IDs")
      return(tools_df)
    }
    
    # Method 2: Try to find tool data in JavaScript objects
    message("Trying to extract tool data from JavaScript objects...")
    data_js <- '
      // Common patterns for data storage in React apps
      window.__INITIAL_STATE__ || 
      window.__data__ || 
      window.toolsData ||
      []
    '
    
    result2 <- b$Runtime$evaluate(data_js)
    
    if (!is.null(result2$result$value) && length(result2$result$value) > 0) {
      message("Found data in JavaScript objects")
      # Process the data
      return(result2$result$value)
    }
    
    # Method 3: Get the entire page HTML and parse it
    message("Falling back to HTML parsing...")
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    
    # Use rvest to parse
    if (requireNamespace("rvest", quietly = TRUE)) {
      page <- rvest::read_html(html)
      
      # Look for links with tool IDs
      tool_links <- rvest::html_nodes(page, "a[href*='/tool?id=']")
      
      if (length(tool_links) > 0) {
        hrefs <- rvest::html_attr(tool_links, "href")
        texts <- rvest::html_text(tool_links, trim = TRUE)
        
        # Extract IDs from URLs
        ids <- gsub(".*\\?id=([0-9]+).*", "\\1", hrefs)
        
        tools_df <- data.frame(
          id = ids,
          name = texts,
          url = hrefs,
          stringsAsFactors = FALSE
        )
        
        message("Found ", nrow(tools_df), " tools")
        return(tools_df)
      }
    }
    
    warning("Could not extract tool data using any method")
    return(NULL)
    
  }, finally = {
    # Clean up: close the browser
    b$close()
    message("Browser closed")
  })
}

#' Find the API endpoint by intercepting network requests
#' 
#' This function uses chromote to intercept network requests and identify
#' the API endpoint that provides tool data.
#' 
#' @param url The URL to monitor
#' @param pattern Pattern to match in request URLs
#' @return List of matching requests
#' 
#' @export
find_api_endpoint <- function(url = "https://biotreasury.cn/toolList",
                               pattern = "api|tool|list") {
  
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("Package 'chromote' is required.")
  }
  
  message("Starting browser to intercept network requests...")
  b <- chromote::ChromoteSession$new()
  
  # Store intercepted requests
  requests <- list()
  
  tryCatch({
    # Enable network monitoring
    b$Network$enable()
    
    # Set up callback to capture requests
    b$Network$requestWillBeSent(function(params) {
      url <- params$request$url
      if (grepl(pattern, url, ignore.case = TRUE)) {
        message("Found matching request: ", url)
        requests <<- c(requests, list(params))
      }
    })
    
    # Navigate to the page
    message("Navigating to ", url)
    b$Page$navigate(url)
    b$Page$loadEventFired()
    
    # Wait for requests to complete
    Sys.sleep(5)
    
    message("Found ", length(requests), " matching requests")
    return(requests)
    
  }, finally = {
    b$close()
  })
}

#' Extract tool ID from biotreasury.cn tool URL
#' 
#' Simple utility function to extract tool ID from a biotreasury.cn URL
#' 
#' @param url A biotreasury.cn tool URL (e.g., "https://biotreasury.cn/tool?id=214144")
#' @return The tool ID as a character string
#' 
#' @export
#' @examples
#' extract_tool_id("https://biotreasury.cn/tool?id=214144")
#' # Returns: "214144"
extract_tool_id <- function(url) {
  # Match the pattern: ?id=NUMBERS
  id <- sub(".*\\?id=([0-9]+).*", "\\1", url)
  
  # Check if extraction was successful
  if (id == url) {
    warning("Could not extract ID from URL: ", url)
    return(NA)
  }
  
  return(id)
}

# Example usage and documentation
if (FALSE) {
  # This code is not run automatically, but serves as documentation
  
  # Check if required packages are installed
  check_packages(required_packages)
  
  # Extract all tools from biotreasury.cn
  tools <- extract_biotreasury_tools()
  
  # View the data
  head(tools)
  
  # Find FaceDig tool
  facedig <- tools[grepl("FaceDig", tools$name, ignore.case = TRUE), ]
  print(facedig)
  
  # Expected output:
  # id: 214144
  # name: FaceDig
  # url: https://biotreasury.cn/tool?id=214144
  
  # Find the API endpoint
  api_requests <- find_api_endpoint()
  
  # Examine the requests to find the tool list API
  lapply(api_requests, function(req) req$request$url)
  
  # Once the API endpoint is identified, you can use it directly:
  # library(httr)
  # library(jsonlite)
  # 
  # response <- GET("https://biotreasury.cn/api/tools/list") # Example
  # tools_data <- fromJSON(content(response, "text"))
}

#' Print information about this script
#' 
#' Display usage information for the biotreasury parsing functions.
#' Call this function to see available functions and examples.
#' 
#' @export
show_biotreasury_info <- function() {
  message("
========================================================================
biotreasury.cn Tool ID Extraction Script
========================================================================

This script provides functions to extract tool IDs from biotreasury.cn.

Key Functions:
- extract_biotreasury_tools(): Main function to extract tool data
- find_api_endpoint(): Discover the API endpoint used by the website
- extract_tool_id(): Extract ID from a tool URL

Requirements:
- chromote: For headless browser automation
- rvest: For HTML parsing
- httr: For HTTP requests
- jsonlite: For JSON parsing

Install with:
  install.packages(c('chromote', 'rvest', 'httr', 'jsonlite'))

Example:
  tools <- extract_biotreasury_tools()
  facedig_id <- tools[tools$name == 'FaceDig', 'id']
  # Expected: '214144'

For more information, see comments in this file.
========================================================================
")
}

# Print info message only when script is run directly (not sourced)
if (sys.nframe() == 0) {
  show_biotreasury_info()
}
