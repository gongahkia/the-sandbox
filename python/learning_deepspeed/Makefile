PYTHON=python3
PIP=pip
REQUIREMENTS=requirements.txt
SCRIPT=run.py

all: install run

install:
	$(PIP) install -r $(REQUIREMENTS)
	@sudo apt install libopenmpi-dev
	@pip install mpi4py

run:
	deepspeed --num_gpus=1 $(SCRIPT) --deepspeed --deepspeed_config ds_config.json --local_rank=0

clean:
	rm -rf __pycache__ *.pyc

.PHONY: all install run clean