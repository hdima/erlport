from erlport import Port, Protocol


class EchoProtocol(Protocol):

    def handle_echo(self, data):
        return data


if __name__ == "__main__":
    proto = EchoProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
