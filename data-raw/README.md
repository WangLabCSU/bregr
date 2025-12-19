# Data Processing Scripts

This directory contains scripts for data processing and web scraping related to the bregr package.

## Files

### biotreasury.cn Tool ID Extraction

#### `parse_biotreasury.R`
Main R script for extracting tool IDs from biotreasury.cn website.

**Key Functions:**
- `extract_biotreasury_tools()`: Extract tool data from biotreasury.cn/toolList
- `find_api_endpoint()`: Discover the API endpoint used by the website
- `extract_tool_id()`: Extract tool ID from a URL

**Usage:**
```r
source("data-raw/parse_biotreasury.R")

# Extract all tools
tools <- extract_biotreasury_tools()

# Find FaceDig tool
facedig <- tools[tools$name == "FaceDig", ]
print(facedig$id)  # Should be "214144"
```

#### `BIOTREASURY_PARSING_GUIDE.md`
Comprehensive guide (in Chinese) explaining the methodology for extracting tool IDs from biotreasury.cn.

Topics covered:
- Website architecture analysis (React SPA)
- Multiple extraction methods:
  1. Browser DevTools to find API endpoints
  2. Headless browser automation
  3. JavaScript source analysis
  4. Network request interception
- Example code in R and Python
- Troubleshooting guide
- Best practices

#### `test_biotreasury_parsing.R`
Test suite for the biotreasury parsing functions.

**Run tests:**
```r
source("data-raw/test_biotreasury_parsing.R")
run_all_tests()
```

Or directly from command line:
```bash
Rscript data-raw/test_biotreasury_parsing.R
```

**Test coverage:**
- ✓ URL parsing (extract_tool_id)
- ✓ Package dependency checking
- ✓ Simulated data extraction
- ✓ HTML parsing (with rvest)
- ✓ JSON parsing (with jsonlite)

## Requirements

### Required R Packages
- `chromote`: Headless browser automation
- `rvest`: HTML parsing
- `httr`: HTTP requests
- `jsonlite`: JSON parsing

Install with:
```r
install.packages(c("chromote", "rvest", "httr", "jsonlite"))
```

### System Requirements
- Chrome or Chromium browser (for chromote)
- Internet connection

## Problem Statement

**Original request (Chinese):**
> 解析 https://biotreasury.cn/toolList，查明如何获取工具的id，比如网站 FaceDig 工具卡片点击后会跳转到 https://biotreasury.cn/tool?id=214144，这个 id 是怎么确定的？

**Translation:**
Parse https://biotreasury.cn/toolList to determine how to obtain tool IDs. For example, when clicking on the FaceDig tool card on the website, it jumps to https://biotreasury.cn/tool?id=214144. How is this ID determined?

## Solution Summary

The biotreasury.cn website is a React single-page application (SPA) that loads tool data dynamically via JavaScript. Tool IDs can be extracted using:

1. **API Endpoint Discovery** (Recommended)
   - Use browser DevTools to find the actual API endpoint
   - Make direct HTTP requests to the API
   - Parse JSON responses to get tool IDs

2. **Headless Browser Automation**
   - Use chromote/Selenium to render JavaScript
   - Extract tool data from rendered DOM
   - Parse tool links to get IDs (format: `?id=XXXXXX`)

3. **Network Request Interception**
   - Monitor network traffic using Chrome DevTools Protocol
   - Capture API calls that return tool data
   - Use the discovered endpoint for future requests

## Example: Finding FaceDig ID

```r
# Method 1: Using simulated data (for testing)
source("data-raw/test_biotreasury_parsing.R")
test_simulated_extraction()

# Method 2: Extract from actual website (requires chromote)
source("data-raw/parse_biotreasury.R")
tools <- extract_biotreasury_tools()
facedig <- tools[grepl("FaceDig", tools$name, ignore.case = TRUE), ]
print(facedig)
# Expected output:
# id: 214144
# name: FaceDig
# url: https://biotreasury.cn/tool?id=214144
```

## Notes

- The `extract_biotreasury_tools()` function requires a headless browser (Chrome/Chromium)
- If the browser is not available, consider finding the API endpoint manually using browser DevTools
- The website structure may change over time; update the CSS selectors or extraction logic accordingly
- Always respect the website's robots.txt and rate limiting

## Contributing

When adding new data processing scripts:
1. Create descriptive filenames
2. Add comprehensive comments and documentation
3. Include test cases
4. Update this README with new scripts

## License

Same as the parent bregr package (GPL >= 3)
