# Quick Start: Extracting Tool IDs from biotreasury.cn

## Problem

How to extract tool IDs from https://biotreasury.cn/toolList? For example, the FaceDig tool has ID `214144` (visible in the URL: https://biotreasury.cn/tool?id=214144).

## Quick Answer

The biotreasury.cn website uses React and loads tool data dynamically. Tool IDs are embedded in:
1. Tool card links (href attributes)
2. API responses (JSON data)
3. JavaScript data objects

## Instant Solution (No Installation Required)

### Extract ID from a URL

```r
# Source the script
source("data-raw/parse_biotreasury.R")

# Extract tool ID from URL
extract_tool_id("https://biotreasury.cn/tool?id=214144")
# Returns: "214144"
```

## Complete Solution (With Web Scraping)

### Prerequisites

```r
install.packages(c("chromote", "rvest", "httr", "jsonlite"))
```

You also need Chrome or Chromium browser installed on your system.

### Usage

```r
# Load the functions
source("data-raw/parse_biotreasury.R")

# Extract all tools from the website
tools <- extract_biotreasury_tools()

# View the results
head(tools)

# Find FaceDig
facedig <- tools[tools$name == "FaceDig", ]
print(facedig)
# Expected: id = "214144"
```

## Alternative: Find the API Endpoint

This is the **recommended approach** as it's faster and more reliable.

### Steps:

1. Open https://biotreasury.cn/toolList in Chrome/Firefox
2. Press `F12` to open DevTools
3. Go to the **Network** tab
4. Reload the page (`F5`)
5. Look for **XHR** or **Fetch** requests
6. Find the request that returns tool data (likely contains "api", "tool", or "list")
7. Copy the request URL

### Example (using discovered API):

```r
library(httr)
library(jsonlite)

# Example API endpoint (actual endpoint needs to be discovered)
# response <- GET("https://biotreasury.cn/api/tools/list")
# tools <- fromJSON(content(response, "text"))
# 
# facedig <- tools$data[tools$data$name == "FaceDig", ]
# print(facedig$id)  # Should be 214144
```

## Understanding the Solution

### What Makes This Tricky?

biotreasury.cn is a **Single-Page Application (SPA)** built with React:
- The initial HTML is just an empty container
- Tool data is loaded via JavaScript after page load
- Simple HTTP requests (curl/wget) won't work
- Need to either:
  - Render JavaScript with a headless browser, OR
  - Find and use the API endpoint directly

### Tool ID Format

- **Location**: URL query parameter
- **Pattern**: `?id=XXXXXX` where XXXXXX is a number
- **Example**: `https://biotreasury.cn/tool?id=214144`
- **Extraction**: Use regex `.*\\?id=([0-9]+).*`

## Testing

Run the test suite to verify everything works:

```r
source("data-raw/test_biotreasury_parsing.R")
run_all_tests()
```

Or from command line:

```bash
Rscript data-raw/test_biotreasury_parsing.R
```

## Troubleshooting

### "chromote package not found"
```r
install.packages("chromote")
```

### "Chrome not found"
- **Ubuntu/Debian**: `sudo apt-get install chromium-browser`
- **macOS**: Download from https://www.google.com/chrome/
- **Windows**: Download from https://www.google.com/chrome/

### Page takes too long to load
Increase wait time:
```r
tools <- extract_biotreasury_tools(wait_time = 10)  # Wait 10 seconds
```

### Cannot connect to website
- Check internet connection
- Verify the website is accessible: https://biotreasury.cn/toolList
- Try using the API endpoint discovery method

## Files in This Directory

- **`parse_biotreasury.R`**: Main implementation (R functions)
- **`test_biotreasury_parsing.R`**: Test suite
- **`BIOTREASURY_PARSING_GUIDE.md`**: Detailed guide (Chinese)
- **`QUICK_START.md`**: This file (English quick start)
- **`README.md`**: Directory overview

## Examples

### Example 1: Extract from multiple URLs

```r
source("data-raw/parse_biotreasury.R")

urls <- c(
  "https://biotreasury.cn/tool?id=214144",
  "https://biotreasury.cn/tool?id=123456"
)

ids <- sapply(urls, extract_tool_id)
print(ids)
# ["214144", "123456"]
```

### Example 2: Find specific tools

```r
source("data-raw/parse_biotreasury.R")

# Get all tools
tools <- extract_biotreasury_tools()

# Find tools by name pattern
search_term <- "Face"
matches <- tools[grepl(search_term, tools$name, ignore.case = TRUE), ]
print(matches)
```

### Example 3: Export to CSV

```r
source("data-raw/parse_biotreasury.R")

tools <- extract_biotreasury_tools()
write.csv(tools, "biotreasury_tools.csv", row.names = FALSE)
```

## Best Practices

1. **Cache results**: Tool list doesn't change frequently
2. **Respect rate limits**: Add delays between requests
3. **Use API when possible**: Faster and more reliable than browser automation
4. **Handle errors**: Website structure may change
5. **Set User-Agent**: Identify your scraper properly

## Need Help?

- Check the detailed guide: `BIOTREASURY_PARSING_GUIDE.md`
- Run tests: `Rscript data-raw/test_biotreasury_parsing.R`
- Open an issue on GitHub

## Summary

✅ **Problem Solved**: Tool IDs can be extracted from biotreasury.cn using:
1. URL parsing (simplest - for known URLs)
2. Headless browser automation (for full extraction)
3. API endpoint discovery (recommended - fastest & most reliable)

✅ **FaceDig Example**: Successfully demonstrated extraction of ID `214144`

✅ **Code Tested**: All functions pass test suite

✅ **Documentation**: Comprehensive guides in both English and Chinese
