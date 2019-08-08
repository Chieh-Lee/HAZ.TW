# HAZ.TW

## Install

```
install.packages("devtools") <br/>
devtools::install_github("Chieh-Lee/HAZ.TW")
```


## Function
`read_fault` : 讀入斷層資料
`output_fault_table` ： 輸出所有斷層資訊
`output_segment_table` ： 輸出特定斷層segment資訊
`plot_all_map` ： 繪製出所有斷層的位置
`plot_segment` ： 繪製特定斷層segment的位置

## How to print the table
```
library(formattable)
formattable(result of `output_fault_table|output_segment_table`)
```