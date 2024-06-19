package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"sync"
)

const (
	cores = 3
)

func run(cmd string) {
	c := exec.Command("sh", "-c", cmd)
	c.Stderr = os.Stderr
	c.Stdout = os.Stdout
	err := c.Run()
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	full := 0xffffffff + 1

	chunksz := full / cores

	var wg sync.WaitGroup
	for i := 0; i < cores; i++ {
		wg.Add(1)
		go func(i int) {
			start := i * chunksz
			end := (i+1)*chunksz - 1
			run(fmt.Sprintf("decl 0x%x 0x%x > decl_out/out_%x_%x.dat", start, end, start, end))
			wg.Done()
		}(i)
	}
	wg.Wait()
}
