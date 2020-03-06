package main

import (
	"fmt"
	"time"
)

const (
	BASE float32 = 5000 // 个税起征点，累计基本减除费用
)

var taxDict [7][3]float32 = [7][3]float32{
	{36000, .03, 0},
	{144000, .10, 2520},
	{300000, .20, 16920},
	{420000, .25, 31920},
	{660000, .30, 52920},
	{960000, .35, 85920},
	{-1, 45, 181920},
}

type income_data struct {
	income          float32 // 税前月收入
	tax_free_income float32 // 当前免税收入
	fund, insurance float32 // 公积金和社保，两项合并为累计专项扣除
	attachAcc       float32 // 专项附加扣除
	others          float32 // 依法确定的其他扣除
	tax             float32 // 徐缴纳所得税
}

func main() {
	income := &income_data{}

	fmt.Printf("请输入你的税前月收入: ")
	fmt.Scanln(&income.income)

	fmt.Printf("请输入你当前的免税收入: ")
	fmt.Scanln(&income.tax_free_income)

	fmt.Printf("请输入你的公积金及社保的个人承担部分（以空格分离）: ")
	fmt.Scanln(&income.fund, &income.insurance)

	fmt.Printf("请输入你的专项附加扣除: ")
	fmt.Scanln(&income.attachAcc)

	fmt.Printf("请输入你依法确定的其他扣除: ")
	fmt.Scanln(&income.others)

	countTax(income)

	fmt.Println("你所需缴纳的个人所得税为: ", income.tax)
}

func countTax(id *income_data) {
	month := int(time.Now().Month())

	fmt.Printf("请输入当前月份: ")
	fmt.Scanln(&month)

	taxUnit := id.income - id.tax_free_income - id.fund - id.insurance - id.attachAcc - BASE
	if id.income < BASE || taxUnit <= 0 {
		return
	}

	for i := 1; i <= month; i++ {
		acc := float32(i) * taxUnit

		for _, lst := range taxDict {
			if id.income <= lst[0] || lst[0] == -1 {
				id.tax += acc*lst[1] - lst[2] - id.tax
				break
			}
		}
	}
}
