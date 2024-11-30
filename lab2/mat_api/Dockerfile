FROM python:3.10-slim


# Install Graphviz
RUN apt-get update && apt-get install -y graphviz

#
WORKDIR /api

COPY requirements.txt .

RUN pip install --upgrade pip

RUN pip install --no-cache-dir -r requirements.txt
#
COPY . .

#
CMD gunicorn main:app --workers 1 --worker-class uvicorn.workers.UvicornWorker --bind=0.0.0.0:8000