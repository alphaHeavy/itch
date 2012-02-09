#!/usr/bin/env python

top = '.'
out = 'build'

def configure(ctx):
  ctx.find_program('ragel', var='RAGEL')
  ctx.find_program('clang', var='CLANG')
  ctx.find_program('llc', var='LLC')
  ctx.find_program('ghc', var='GHC')
  ctx.find_program('sed', var='SED')
  ctx.env.FIXUP    = 's/call void/call cc10 void/; s/define void/define cc10 void/;'
  ctx.env.LLCOPT   = '-O3 -pre-RA-sched=list-burr -regalloc=greedy -relocation-model=static'
  ctx.env.CLANGOPT = '-O3 -Wall -Wno-uninitialized'
  ctx.env.GHCOPT   = '-O2 -Wall -rtsopts'

def build(bld):
  bld(rule='${RAGEL} -G2 ${SRC} -o ${TGT}',                          source='ITCHv41.rl',         target='ITCHv41.c')
  bld(rule='${CLANG} ${CLANGOPT} -emit-llvm -S -c ${SRC} -o ${TGT}', source='ITCHv41.c',          target='ITCHv41.ll')
  bld(rule='${SED} -e "${FIXUP}" < ${SRC} > ${TGT}',                 source='ITCHv41.ll',         target='ITCHv41.ll-patched')
  bld(rule='${LLC} ${LLCOPT} -filetype=obj ${SRC} -o ${TGT}',        source='ITCHv41.ll-patched', target='ITCHv41.o')
  bld(rule='${GHC} ${GHCOPT} --make -outputdir=. ${SRC} -o ${TGT}',  source=['ITCHv41.o', 'Parser.hs', 'Main.hs'], target='parser')

