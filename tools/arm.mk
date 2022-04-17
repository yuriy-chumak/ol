# --------------------------------------------------------------------
# | ARM Core | Command Line Options                       | multilib |
# |----------|--------------------------------------------|----------|
# |Cortex-M0+| -mthumb -mcpu=cortex-m0plus                | armv6-m  |
# |Cortex-M0 | -mthumb -mcpu=cortex-m0                    |          |
# |Cortex-M1 | -mthumb -mcpu=cortex-m1                    |          |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv6-m                     |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M3 | -mthumb -mcpu=cortex-m3                    | armv7-m  |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv7-m                     |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M4 | -mthumb -mcpu=cortex-m4                    | armv7e-m |
# |(No FP)   |--------------------------------------------|          |
# |          | -mthumb -march=armv7e-m                    |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M4 | -mthumb -mcpu=cortex-m4 -mfloat-abi=softfp | armv7e-m |
# |(Soft FP) | -mfpu=fpv4-sp-d16                          | /softfp  |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv7e-m -mfloat-abi=softfp |          |
# |          | -mfpu=fpv4-sp-d16                          |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M4 | -mthumb -mcpu=cortex-m4 -mfloat-abi=hard   | armv7e-m |
# |(Hard FP) | -mfpu=fpv4-sp-d16                          | /fpu     |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv7e-m -mfloat-abi=hard   |          |
# |          | -mfpu=fpv4-sp-d16                          |          |
# |----------|--------------------------------------------|----------|
# |Cortex-R4 | [-mthumb] -march=armv7-r                   | armv7-ar |
# |Cortex-R5 |                                            | /thumb   |
# |Cortex-R7 |                                            |          |
# |(No FP)   |                                            |          |
# |----------|--------------------------------------------|----------|
# |Cortex-R4 | [-mthumb] -march=armv7-r -mfloat-abi=softfp| armv7-ar |
# |Cortex-R5 | -mfpu=vfpv3-d16                            | /thumb   |
# |Cortex-R7 |                                            | /softfp  |
# |(Soft FP) |                                            |          |
# |----------|--------------------------------------------|----------|
# |Cortex-R4 | [-mthumb] -march=armv7-r -mfloat-abi=hard  | armv7-ar |
# |Cortex-R5 | -mfpu=vfpv3-d16                            | /thumb   |
# |Cortex-R7 |                                            | /fpu     |
# |(Hard FP) |                                            |          |
# |----------|--------------------------------------------|----------|
# |Cortex-A* | [-mthumb] -march=armv7-a                   | armv7-ar |
# |(No FP)   |                                            | /thumb   |
# |----------|--------------------------------------------|----------|
# |Cortex-A* | [-mthumb] -march=armv7-a -mfloat-abi=softfp| armv7-ar |
# |(Soft FP) | -mfpu=vfpv3-d16                            | /thumb   |
# |          |                                            | /softfp  |
# |----------|--------------------------------------------|----------|
# |Cortex-A* | [-mthumb] -march=armv7-a -mfloat-abi=hard  | armv7-ar |
# |(Hard FP) | -mfpu=vfpv3-d16                            | /thumb   |
# |          |                                            | /fpu     |
# --------------------------------------------------------------------

arm: src/olvm.c
	arm-linux-gnueabihf-gcc-5 src/olvm.c -o $@ \
	   -Xlinker --export-dynamic $(L) \
	   -march=armv7-a -mfpu=neon-vfpv4 \
	   $(CFLAGS)
	@echo Ok.

#-mcpu=cortex-m3
#	   -mfpu=vfp \
# -mthumb -mno-thumb-interwork -mfpu=vfp -msoft-float -mfix-cortex-m3-ldrd \
