FROM python:3

ADD tcpToMqtt.py /

RUN pip install paho-mqtt

EXPOSE 2102

ENTRYPOINT ["python", "./tcpToMqtt.py"]
