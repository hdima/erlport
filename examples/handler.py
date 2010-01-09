from erlport import Port, Protocol
from erlport import Atom


class EventHandler(Protocol):

    def __init__(self):
        self.collected = []

    def handle(self, port, message):
        if message == Atom("stop"):
            port.write(self.collected)
            self.collected = []
        else:
            self.collected.append(message)


if __name__ == "__main__":
    proto = EventHandler()
    proto.run(Port(packet=4, use_stdio=True))
