from erlport import Port, Protocol, String


class HelloProtocol(Protocol):

    def handle_hello(self, name):
        return "Hello, %s" % String(name)


if __name__ == "__main__":
    proto = HelloProtocol()
    proto.run(Port(use_stdio=True))
