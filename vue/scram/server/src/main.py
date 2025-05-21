from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
import docker

client = docker.from_env()
app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.post("/execute")
async def execute_code(language: str, code: str):
    try:
        container = client.containers.run(
            image=f"{language}-runner",
            command=f"timeout 5 {language}-runner",
            mem_limit="100m",
            network_mode="none",
            detach=True,
            volumes={'code': {'bind': '/app', 'mode': 'ro'}}
        )
        result = container.wait()
        logs = container.logs().decode('utf-8')
        container.remove()
        return {"output": logs}
    except docker.errors.DockerException as e:
        raise HTTPException(500, str(e))