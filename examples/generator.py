import time

from erlport import Port, Protocol
from erlport import Atom


class EventGenerator(Protocol):

    def handle(self, port, n):
        while n > 0:
            port.write(time.time())
            n -= 1
        port.write(Atom("stop"))


if __name__ == "__main__":
    proto = EventGenerator()
    proto.run(Port(use_stdio=True))
