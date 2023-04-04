package main

import (
	"bufio"
	"fmt"
	"io"
	"os"

	"github.com/topxeq/qoxe"
	"github.com/topxeq/tk"
)

func runInteractiveShell() int {
	tk.Pl(`Qoxe V%v`, qoxe.VersionG)
	qoxe.GlobalsG.Vars["ShellModeG"] = true
	qoxe.GlobalsG.Vars["leSilentG"] = true

	var following bool
	var source string
	scanner := bufio.NewScanner(os.Stdin)

	// vm0T := qoxe.NewVM()

	// if tk.IsError(vm0T) {
	// 	tk.Pl("failed to initialize VM(初始化虚拟机失败): %v", tk.GetErrStrX(vm0T))
	// 	os.Exit(1)
	// }

	// vmT := vm0T.(*xie.XieVM)

	// vmT.SetVar(vmT.Running, "argsG", os.Args)

	// guiHandlerG = guiHandler

	// vmT.SetVar(vmT.Running, "guiG", guiHandlerG)

	for {
		if following {
			source += "\n"
			fmt.Print("  ")
		} else {
			fmt.Print("> ")
		}

		if !scanner.Scan() {
			break
		}
		source += scanner.Text()
		if source == "" {
			continue
		}

		if source == "quit" {
			break
		} else if source == "#debug" {
			// vmT.Debug()
			following = false
			source = ""
			continue
		}

		// retG := ""

		rs := qoxe.RunCode(source, nil)

		tk.Pl("rs: %v", rs)

		// originalCodeLenT := vmT.GetCodeLen(vmT.Running)

		// lrs := vmT.Load(vmT.Running, source)

		// if tk.IsError(lrs) {
		// 	following = false
		// 	source = ""
		// 	fmt.Println("failed to load source code of the script(载入代码失败): ", tk.GetErrStrX(lrs))
		// 	continue
		// }

		// rs := vmT.Run(originalCodeLenT)

		// noResultT := tk.IsUndefined(rs) // == "TXERROR:no result")

		// if tk.IsErrX(rs) {
		// 	fmt.Fprintln(os.Stderr, "failed to run(运行失败): "+tk.GetErrStrX(rs))
		// 	following = false
		// 	source = ""
		// 	continue
		// }

		// if !noResultT {
		// 	fmt.Println(retG)
		// }

		following = false
		source = ""
	}

	if err := scanner.Err(); err != nil {
		if err != io.EOF {
			fmt.Fprintln(os.Stderr, "failed to read char(获取键盘输入失败):", err)
			return 12
		}
	}

	return 0
}

func main() {
	codeT := `
	let five = 5;
	let ten = 10.2;
	let b = false;
	`
	rs := qoxe.RunCode(codeT, nil)

	tk.Pl("rs: %v", rs)

	return

	// 	codeT := `
	// 	let five = 5;
	// let ten = 10;
	// let add = fn(x, y) {
	//   x + y;
	// };
	// let result = add(five, ten);
	// !-/*5;
	// 5 < 10 > 5;
	// if (5 < 10) {
	// return true;
	// } else {
	// return false;
	// }
	// 10 == 10;
	// 10 != 9;
	// 	`
	// 	rs := qoxe.RunCode(codeT, nil)

	// 	tk.Pl("rs: %v", rs)

	// runInteractiveShell()
}
