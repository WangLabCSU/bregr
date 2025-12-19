# Test script for biotreasury.cn parsing functions
# 
# This script tests the tool ID extraction functions with example data
# and provides usage examples.

# Source the main parsing script
source("data-raw/parse_biotreasury.R")

# Test 1: Extract tool ID from URL
test_extract_tool_id <- function() {
  cat("=== Testing extract_tool_id() ===\n")
  
  # Test cases
  test_urls <- c(
    "https://biotreasury.cn/tool?id=214144",
    "https://biotreasury.cn/tool?id=123456&param=value",
    "/tool?id=789012",
    "invalid-url"
  )
  
  expected_ids <- c("214144", "123456", "789012", NA)
  
  for (i in seq_along(test_urls)) {
    url <- test_urls[i]
    expected <- expected_ids[i]
    result <- extract_tool_id(url)
    
    if (is.na(expected) && is.na(result)) {
      status <- "✓ PASS"
    } else if (!is.na(expected) && !is.na(result) && result == expected) {
      status <- "✓ PASS"
    } else {
      status <- "✗ FAIL"
    }
    
    cat(sprintf("%s: URL: %s -> ID: %s (expected: %s)\n", 
                status, url, result, expected))
  }
  
  cat("\n")
}

# Test 2: Check required packages
test_check_packages <- function() {
  cat("=== Testing check_packages() ===\n")
  
  # Test with a mix of installed and non-installed packages
  test_packages <- c("base", "stats", "this_package_probably_does_not_exist_12345")
  
  cat("Checking packages: ", paste(test_packages, collapse = ", "), "\n")
  check_packages(test_packages)
  
  cat("\n")
}

# Test 3: Simulate tool data extraction (without actual web request)
test_simulated_extraction <- function() {
  cat("=== Testing simulated tool data ===\n")
  
  # Simulate the data structure that would be returned from the website
  simulated_tools <- data.frame(
    id = c("214144", "123456", "789012"),
    name = c("FaceDig", "ToolX", "ToolY"),
    url = c(
      "https://biotreasury.cn/tool?id=214144",
      "https://biotreasury.cn/tool?id=123456",
      "https://biotreasury.cn/tool?id=789012"
    ),
    stringsAsFactors = FALSE
  )
  
  cat("Simulated tool data:\n")
  print(simulated_tools)
  
  # Test finding FaceDig
  facedig <- simulated_tools[simulated_tools$name == "FaceDig", ]
  cat("\nFinding FaceDig:\n")
  print(facedig)
  
  if (nrow(facedig) == 1 && facedig$id == "214144") {
    cat("✓ PASS: FaceDig has correct ID (214144)\n")
  } else {
    cat("✗ FAIL: FaceDig ID mismatch\n")
  }
  
  cat("\n")
}

# Test 4: HTML parsing example with simulated HTML
test_html_parsing <- function() {
  cat("=== Testing HTML parsing ===\n")
  
  # Simulated HTML that might be found on the page
  simulated_html <- '
  <html>
    <body>
      <div class="tool-list">
        <a href="/tool?id=214144" class="tool-card">FaceDig</a>
        <a href="/tool?id=123456" class="tool-card">ToolX</a>
        <a href="/tool?id=789012" class="tool-card">ToolY</a>
      </div>
    </body>
  </html>
  '
  
  if (requireNamespace("rvest", quietly = TRUE)) {
    library(rvest)
    
    # Parse the HTML
    page <- read_html(simulated_html)
    
    # Extract tool links
    tool_links <- html_nodes(page, "a.tool-card")
    hrefs <- html_attr(tool_links, "href")
    names <- html_text(tool_links, trim = TRUE)
    
    # Extract IDs
    ids <- sapply(hrefs, extract_tool_id)
    
    # Create data frame
    tools_df <- data.frame(
      id = ids,
      name = names,
      url = hrefs,
      stringsAsFactors = FALSE
    )
    
    cat("Extracted tools from HTML:\n")
    print(tools_df)
    
    # Verify FaceDig
    facedig <- tools_df[tools_df$name == "FaceDig", ]
    if (nrow(facedig) == 1 && facedig$id == "214144") {
      cat("✓ PASS: HTML parsing extracted FaceDig correctly\n")
    } else {
      cat("✗ FAIL: HTML parsing failed\n")
    }
  } else {
    cat("⚠ SKIP: rvest package not installed\n")
  }
  
  cat("\n")
}

# Test 5: JSON parsing example
test_json_parsing <- function() {
  cat("=== Testing JSON parsing ===\n")
  
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    library(jsonlite)
    
    # Simulated API response
    simulated_json <- '{
      "status": "success",
      "data": [
        {
          "id": 214144,
          "name": "FaceDig",
          "description": "A tool for...",
          "url": "https://biotreasury.cn/tool?id=214144"
        },
        {
          "id": 123456,
          "name": "ToolX",
          "description": "Another tool...",
          "url": "https://biotreasury.cn/tool?id=123456"
        }
      ]
    }'
    
    # Parse JSON
    parsed <- fromJSON(simulated_json)
    tools_df <- parsed$data
    
    cat("Extracted tools from JSON:\n")
    print(tools_df)
    
    # Verify FaceDig
    facedig <- tools_df[tools_df$name == "FaceDig", ]
    if (nrow(facedig) == 1 && facedig$id == 214144) {
      cat("✓ PASS: JSON parsing extracted FaceDig correctly\n")
    } else {
      cat("✗ FAIL: JSON parsing failed\n")
    }
  } else {
    cat("⚠ SKIP: jsonlite package not installed\n")
  }
  
  cat("\n")
}

# Main test runner
run_all_tests <- function() {
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("  biotreasury.cn Tool ID Extraction - Test Suite\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("\n")
  
  test_extract_tool_id()
  test_check_packages()
  test_simulated_extraction()
  test_html_parsing()
  test_json_parsing()
  
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("  Test suite completed\n")
  cat("=" |> rep(70) |> paste(collapse = ""), "\n")
  cat("\n")
  cat("To actually extract data from biotreasury.cn, use:\n")
  cat("  tools <- extract_biotreasury_tools()\n")
  cat("\n")
  cat("Note: This requires chromote package and Chrome/Chromium browser\n")
  cat("Install with: install.packages('chromote')\n")
  cat("\n")
}

# Run tests if this script is executed directly
if (sys.nframe() == 0) {
  run_all_tests()
}
