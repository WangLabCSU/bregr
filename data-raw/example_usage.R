#!/usr/bin/env Rscript
# Example Usage: Extract tool IDs from biotreasury.cn
#
# This script demonstrates the solution to:
# "解析 https://biotreasury.cn/toolList，查明如何获取工具的id"
# (Parse biotreasury.cn/toolList to determine how to obtain tool IDs)
#
# Run with: Rscript data-raw/example_usage.R

# Load the parsing functions
source("data-raw/parse_biotreasury.R")

cat("\n")
cat("================================================================\n")
cat("  biotreasury.cn Tool ID Extraction - Example Usage\n")
cat("================================================================\n")
cat("\n")

# ============================================================
# Example 1: Extract ID from a URL
# ============================================================
cat("Example 1: Extract tool ID from URL\n")
cat("------------------------------------\n")

url_facedig <- "https://biotreasury.cn/tool?id=214144"
id_facedig <- extract_tool_id(url_facedig)

cat("Input URL:  ", url_facedig, "\n")
cat("Extracted ID:", id_facedig, "\n")
cat("✓ Success: FaceDig tool has ID", id_facedig, "\n")
cat("\n")

# ============================================================
# Example 2: Batch extract IDs from multiple URLs
# ============================================================
cat("Example 2: Batch extract IDs from multiple URLs\n")
cat("------------------------------------------------\n")

tool_urls <- c(
  "https://biotreasury.cn/tool?id=214144",  # FaceDig
  "https://biotreasury.cn/tool?id=100001",  # Example Tool 1
  "https://biotreasury.cn/tool?id=100002",  # Example Tool 2
  "/tool?id=999999"                          # Relative URL
)

cat("Processing", length(tool_urls), "URLs:\n\n")

for (url in tool_urls) {
  id <- extract_tool_id(url)
  cat(sprintf("  %-50s -> ID: %s\n", url, id))
}

cat("\n✓ Success: Extracted", length(tool_urls), "tool IDs\n")
cat("\n")

# ============================================================
# Example 3: Work with simulated tool data
# ============================================================
cat("Example 3: Simulate tool data extraction\n")
cat("-----------------------------------------\n")

# This simulates what you would get from extract_biotreasury_tools()
simulated_tools <- data.frame(
  id = c("214144", "100001", "100002", "100003", "100004"),
  name = c("FaceDig", "ToolAlpha", "ToolBeta", "ToolGamma", "ToolDelta"),
  description = c(
    "Face recognition tool",
    "Alpha analysis tool",
    "Beta testing tool",
    "Gamma correction tool",
    "Delta variant analyzer"
  ),
  url = c(
    "https://biotreasury.cn/tool?id=214144",
    "https://biotreasury.cn/tool?id=100001",
    "https://biotreasury.cn/tool?id=100002",
    "https://biotreasury.cn/tool?id=100003",
    "https://biotreasury.cn/tool?id=100004"
  ),
  stringsAsFactors = FALSE
)

cat("\nSimulated tool database:\n")
print(simulated_tools)

cat("\n--- Search for specific tools ---\n\n")

# Search for FaceDig
cat("1. Find FaceDig:\n")
facedig <- simulated_tools[simulated_tools$name == "FaceDig", ]
cat("   Name:", facedig$name, "\n")
cat("   ID:  ", facedig$id, "\n")
cat("   URL: ", facedig$url, "\n")

# Search by pattern
cat("\n2. Find all tools with 'Tool' in name:\n")
tool_pattern <- simulated_tools[grepl("Tool", simulated_tools$name), ]
cat("   Found", nrow(tool_pattern), "tools:\n")
for (i in 1:nrow(tool_pattern)) {
  cat(sprintf("   - %s (ID: %s)\n", tool_pattern$name[i], tool_pattern$id[i]))
}

# Get IDs only
cat("\n3. Extract all IDs:\n")
all_ids <- simulated_tools$id
cat("   IDs:", paste(all_ids, collapse = ", "), "\n")

cat("\n✓ Success: Demonstrated data manipulation\n")
cat("\n")

# ============================================================
# Example 4: Instructions for real website extraction
# ============================================================
cat("Example 4: How to extract from the actual website\n")
cat("--------------------------------------------------\n")
cat("\n")
cat("To extract tool data from the real biotreasury.cn website:\n")
cat("\n")
cat("Method 1: Use headless browser (automated)\n")
cat("  1. Install chromote: install.packages('chromote')\n")
cat("  2. Ensure Chrome/Chromium is installed\n")
cat("  3. Run the following code:\n")
cat("\n")
cat('     tools <- extract_biotreasury_tools()\n')
cat('     facedig <- tools[tools$name == "FaceDig", ]\n')
cat('     print(facedig$id)  # Should show: 214144\n')
cat("\n")
cat("Method 2: Find API endpoint (recommended)\n")
cat("  1. Open https://biotreasury.cn/toolList in browser\n")
cat("  2. Press F12 to open DevTools\n")
cat("  3. Go to Network tab, filter by XHR/Fetch\n")
cat("  4. Reload page and look for API requests\n")
cat("  5. Find the request returning tool data\n")
cat("  6. Use that endpoint with httr/curl\n")
cat("\n")
cat("Example API usage (once endpoint is discovered):\n")
cat("\n")
cat('  library(httr)\n')
cat('  library(jsonlite)\n')
cat('  \n')
cat('  # Example (replace with actual API endpoint)\n')
cat('  response <- GET("https://biotreasury.cn/api/tools/list")\n')
cat('  tools <- fromJSON(content(response, "text"))\n')
cat('  \n')
cat('  # Find FaceDig\n')
cat('  facedig <- tools$data[tools$data$name == "FaceDig", ]\n')
cat('  print(facedig$id)  # Expected: 214144\n')
cat("\n")

# ============================================================
# Summary
# ============================================================
cat("================================================================\n")
cat("  Summary\n")
cat("================================================================\n")
cat("\n")
cat("✓ Demonstrated URL parsing to extract tool IDs\n")
cat("✓ Showed batch processing of multiple URLs\n")
cat("✓ Illustrated data manipulation and search\n")
cat("✓ Provided instructions for real website extraction\n")
cat("\n")
cat("Key Finding:\n")
cat("  FaceDig tool ID is: 214144\n")
cat("  URL format: https://biotreasury.cn/tool?id={ID}\n")
cat("\n")
cat("Documentation:\n")
cat("  - parse_biotreasury.R          (Implementation)\n")
cat("  - BIOTREASURY_PARSING_GUIDE.md (Detailed guide in Chinese)\n")
cat("  - QUICK_START.md               (Quick start in English)\n")
cat("  - test_biotreasury_parsing.R   (Test suite)\n")
cat("\n")
cat("Next Steps:\n")
cat("  1. Review the documentation files\n")
cat("  2. Run tests: Rscript data-raw/test_biotreasury_parsing.R\n")
cat("  3. Try extracting from the real website with chromote\n")
cat("  4. Or find the API endpoint using browser DevTools\n")
cat("\n")
cat("================================================================\n")
cat("\n")
