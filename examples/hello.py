from erlport import Port, Protocol


class HelloProtocol(Protocol):

    def handle_hello(self, name):
        return "Hello, %s" % name


if __name__ == "__main__":
    proto = HelloProtocol()
    proto.run(Port())
