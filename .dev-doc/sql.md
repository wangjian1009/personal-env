# 常用SQL语句
[TOC]

### 数据库相关
- 列举所有db: `show databases`

### 表相关
- 列举所有表格: `show tables`
- 查看表结构: `describe {table_name}`
- 查看索引: `show index from {table_name}`

### 环境相关
- 查看时区: `SELECT TIMEDIFF(NOW(), UTC_TIMESTAMP)`

### 函数
#### 时间日期
