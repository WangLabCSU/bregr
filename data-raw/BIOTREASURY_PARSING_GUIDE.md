# 如何获取 biotreasury.cn 工具的 ID

## 问题描述

biotreasury.cn 是一个生物信息学工具的整合资源库。当点击工具卡片（如 FaceDig）时，会跳转到对应的详情页，例如：
- FaceDig 工具：`https://biotreasury.cn/tool?id=214144`

本文档说明如何从网站中提取这些工具 ID。

## 网站架构分析

### 1. 技术栈
biotreasury.cn 使用了以下技术：
- **React**: 前端框架，构建单页应用 (SPA)
- **动态加载**: 内容通过 JavaScript 动态渲染
- **API 调用**: 数据通过 AJAX/Fetch 从后端 API 获取

### 2. HTML 结构
访问 `https://biotreasury.cn/toolList` 返回的初始 HTML 只包含：
```html
<div id="reactRoot"></div>
<script src="..."></script>
```

这意味着工具列表数据不在初始 HTML 中，而是通过 JavaScript 动态加载的。

## 提取工具 ID 的方法

### 方法 1: 使用浏览器开发者工具查找 API 端点（推荐）

这是最直接的方法，可以找到网站使用的真实 API 端点。

**步骤：**

1. **打开浏览器开发者工具**
   - Chrome/Edge: 按 `F12` 或右键点击页面选择"检查"
   - Firefox: 按 `F12` 或 `Ctrl+Shift+I`

2. **切换到 Network（网络）标签**
   - 在开发者工具中找到 "Network" 或"网络"标签

3. **访问工具列表页面**
   - 在浏览器地址栏输入：`https://biotreasury.cn/toolList`
   - 或刷新页面（`F5`）以重新加载

4. **查找 XHR/Fetch 请求**
   - 在 Network 标签中，点击 "XHR" 或 "Fetch" 过滤器
   - 查找包含工具数据的请求（通常包含 "tool", "list", "api" 等关键词）

5. **查看响应数据**
   - 点击找到的请求
   - 在 "Preview" 或 "Response" 标签中查看返回的 JSON 数据
   - 数据中应该包含工具的 ID、名称等信息

6. **记录 API 端点**
   - 复制请求的完整 URL（例如：`https://biotreasury.cn/api/tools/list`）
   - 记录请求方法（GET/POST）和必要的请求头

**示例：**
假设找到的 API 端点是 `https://biotreasury.cn/api/tools/list`，返回的 JSON 格式如下：
```json
{
  "data": [
    {
      "id": 214144,
      "name": "FaceDig",
      "description": "...",
      "url": "https://biotreasury.cn/tool?id=214144"
    },
    ...
  ]
}
```

### 方法 2: 使用无头浏览器（Headless Browser）

如果无法找到 API 端点，或 API 需要复杂的认证，可以使用无头浏览器来渲染页面并提取数据。

**R 语言实现：**

```r
# 安装所需包
install.packages("chromote")

# 使用 chromote
library(chromote)

# 创建浏览器实例
b <- ChromoteSession$new()

# 访问页面
b$Page$navigate("https://biotreasury.cn/toolList")
b$Page$loadEventFired()

# 等待页面加载
Sys.sleep(5)

# 提取所有工具链接
links_js <- '
  Array.from(document.querySelectorAll("a[href*=\'/tool?id=\']"))
    .map(a => ({
      href: a.href,
      id: new URLSearchParams(a.href.split("?")[1]).get("id"),
      text: a.textContent.trim()
    }))
'

result <- b$Runtime$evaluate(links_js)

# 清理
b$close()

# 处理结果
tools <- result$result$value
```

**Python 实现：**

```python
from selenium import webdriver
from selenium.webdriver.common.by import By
import time

# 创建无头浏览器
options = webdriver.ChromeOptions()
options.add_argument('--headless')
driver = webdriver.Chrome(options=options)

# 访问页面
driver.get('https://biotreasury.cn/toolList')
time.sleep(5)  # 等待页面加载

# 提取所有工具链接
tool_links = driver.find_elements(By.CSS_SELECTOR, "a[href*='/tool?id=']")

tools = []
for link in tool_links:
    href = link.get_attribute('href')
    text = link.text
    tool_id = href.split('id=')[1] if 'id=' in href else None
    tools.append({'id': tool_id, 'name': text, 'url': href})

# 清理
driver.quit()

# 显示结果
for tool in tools:
    print(f"ID: {tool['id']}, Name: {tool['name']}")
```

### 方法 3: 分析 JavaScript 源码

如果工具数据在页面加载时就包含在 JavaScript 变量中，可以直接从源码提取。

**步骤：**

1. 访问 `https://biotreasury.cn/toolList`
2. 查看页面源代码（`Ctrl+U`）
3. 搜索 JavaScript 文件中的数据模式，例如：
   - `window.__INITIAL_STATE__`
   - `window.__data__`
   - `toolsData`
4. 如果找到，可以直接解析这些数据

### 方法 4: 监听网络请求（高级）

使用浏览器的 CDP (Chrome DevTools Protocol) 监听所有网络请求。

```r
library(chromote)

b <- ChromoteSession$new()

# 启用网络监听
b$Network$enable()

# 存储请求
requests <- list()

# 设置回调
b$Network$requestWillBeSent(function(params) {
  url <- params$request$url
  if (grepl("api|tool|list", url, ignore.case = TRUE)) {
    cat("Found request:", url, "\n")
    requests <<- c(requests, list(params))
  }
})

# 访问页面
b$Page$navigate("https://biotreasury.cn/toolList")
b$Page$loadEventFired()
Sys.sleep(5)

# 查看捕获的请求
lapply(requests, function(req) req$request$url)

b$close()
```

## 工具 ID 的特征

通过分析可以发现：

1. **URL 格式**: `https://biotreasury.cn/tool?id={ID}`
2. **ID 格式**: 数字，例如 `214144`
3. **唯一性**: 每个工具有唯一的 ID
4. **获取方式**: 
   - 从工具列表的链接 `href` 属性中提取
   - 从 API 响应的 JSON 数据中获取
   - 从页面的数据属性（如 `data-id`）中提取

## 示例：查找 FaceDig 的 ID

### 使用 R 脚本

```r
source("data-raw/parse_biotreasury.R")

# 提取所有工具
tools <- extract_biotreasury_tools()

# 查找 FaceDig
facedig <- tools[grepl("FaceDig", tools$name, ignore.case = TRUE), ]

print(facedig)
# 输出:
# id      name     url
# 214144  FaceDig  https://biotreasury.cn/tool?id=214144
```

### 使用命令行工具

如果已知 API 端点：

```bash
# 使用 curl 获取工具列表
curl -s 'https://biotreasury.cn/api/tools/list' | jq '.data[] | select(.name == "FaceDig")'

# 输出应包含:
# {
#   "id": 214144,
#   "name": "FaceDig",
#   ...
# }
```

## 最佳实践

1. **优先使用 API**: 如果能找到 API 端点，直接调用 API 是最高效的方法
2. **缓存数据**: 工具列表通常不会频繁变化，可以缓存结果避免频繁请求
3. **遵守爬虫规则**: 
   - 检查 `robots.txt`: `https://biotreasury.cn/robots.txt`
   - 添加合理的请求间隔
   - 设置适当的 User-Agent
4. **错误处理**: 网站结构可能变化，代码应该有健壮的错误处理
5. **联系网站管理员**: 如果需要大量数据，考虑联系网站提供方获取数据访问权限

## 技术要求

### R 语言
- `chromote`: 无头浏览器控制
- `rvest`: HTML 解析
- `httr`: HTTP 请求
- `jsonlite`: JSON 解析

### Python
- `selenium`: 浏览器自动化
- `beautifulsoup4`: HTML 解析
- `requests`: HTTP 请求

### 系统要求
- Chrome 或 Chromium 浏览器（用于无头浏览器方法）
- 稳定的网络连接

## 故障排查

### 问题：页面加载后没有数据

**原因：** JavaScript 渲染需要时间

**解决：** 增加等待时间
```r
Sys.sleep(10)  # 等待更长时间
```

### 问题：无法找到工具链接

**原因：** 
- 页面结构变化
- CSS 选择器不正确
- 数据通过其他方式加载

**解决：**
1. 使用浏览器检查工具查看实际的 HTML 结构
2. 更新 CSS 选择器
3. 尝试其他提取方法

### 问题：API 请求失败

**原因：**
- 需要认证令牌
- 需要特定的请求头
- IP 被限制

**解决：**
1. 在浏览器开发者工具中查看请求的完整信息
2. 复制所有必要的请求头
3. 考虑使用 cookies 或认证令牌

## 总结

获取 biotreasury.cn 工具 ID 的关键步骤：

1. ✅ 识别网站使用 React/SPA 技术
2. ✅ 使用浏览器开发者工具查找 API 端点
3. ✅ 使用无头浏览器渲染页面并提取数据
4. ✅ 从 URL 参数中解析工具 ID（格式：`?id=XXXXXX`）

建议的工作流程：
1. 首先尝试找到 API 端点（最快最稳定）
2. 如果无法找到，使用无头浏览器
3. 实现数据缓存以提高效率
4. 添加错误处理和日志记录

## 参考资源

- [chromote 包文档](https://github.com/rstudio/chromote)
- [rvest 包文档](https://rvest.tidyverse.org/)
- [Selenium 文档](https://www.selenium.dev/documentation/)
- [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)

## 联系信息

如有问题或需要帮助，请联系：
- biotreasury.cn 网站管理员
- 本项目维护者
