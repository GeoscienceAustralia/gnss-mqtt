import socket
import threading
import multiprocessing as mp
import logging
import uuid
import paho.mqtt.client as mqtt
import time

logging.basicConfig(level=logging.DEBUG)#, filename="/opt/streamforwarder/output.log")

class MqttClient(mqtt.Client):
    def __init__(self, host='127.0.0.1', port=1883, *args, **kwargs):
        mqtt.Client.__init__(self, *args, **kwargs)

        # Unfortunately mqtt.Client's __init__ method overwrites these values
        # So they can't be MqttClient attributes or functions
        self.on_connect = self._on_connect
        self.on_disconnect = lambda: logging.error('MQTT client disconnected')

        self.connect(host, port)
        self.loop_start()

    @staticmethod
    def _on_connect(client, userdata, flags, rc):
        if rc != 0:
            logging.error('MQTT client connection failed with code {}'.format(rc))

        else:
            logging.info('MQTT client connection succeeded')


def socket_to_mqtt(socket, ip_address, mqtt_client):
    client_id = '{}-{}'.format(ip_address, uuid.uuid4())

    while True:
        data = socket.recv(1024)
        if not data:
            logging.error('{} source disconnected'.format(client_id))
            return

        mqtt_client.publish(client_id, payload=bytearray(data), qos=1)


def tcp_handler(bind_ip='0.0.0.0', bind_port=2102):
    mqtt_client = MqttClient(client_id=str(uuid.uuid4()), clean_session=False)

    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind((bind_ip, bind_port))
    server.listen(5)
    logging.info('Listening on {}:{}'.format(bind_ip, bind_port))

    while True:
        client_socket, address = server.accept()
        logging.info('Accepted connection from {}:{}'.format(address[0], address[1]))
        client_handler = threading.Thread(
            target=socket_to_mqtt,
            args=(client_socket, address[0], mqtt_client))
        client_handler.start()


if __name__ == '__main__':
    tcp_handler()
