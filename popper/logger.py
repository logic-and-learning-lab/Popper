# logger.py
import logging

# 1. Create the instance
logger = logging.getLogger("popper")

# 2. Add your custom logic here (Formatter, Handlers, etc.)
class SecondsFormatter(logging.Formatter):
    def format(self, record):
        record.elapsed_secs = record.relativeCreated / 1000.0
        return super().format(record)

# Setup once
if not logger.handlers:
    handler = logging.StreamHandler()
    formatter = SecondsFormatter("%(elapsed_secs).1fs %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    logger.propagate = False

# 3. Export the methods or the object itself
info = logger.info
debug = logger.debug
warning = logger.warning
error = logger.error
setLevel = logger.setLevel