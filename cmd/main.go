package main

import (
	"github.com/topxeq/qoxe"
	"github.com/topxeq/tk"
)

func main() {
	rs := qoxe.RunCode("a")

	tk.Pl("rs: %v", rs)
}
