# logger.py
import logging

# 1. Create the instance
logger = logging.getLogger("popper")
# logger.setLevel(logging.DEBUG)

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


# Add this to logger.py
def set_verbosity(level):
    """
    1 = Normal (out)
    2 = Info (info + out)
    3 = Detailed (debug + info + out)
    """
    if level == 1:
        logger.setLevel(logging.WARNING)
    elif level == 2:
        logger.setLevel(logging.INFO)
    elif level == 3:
        logger.setLevel(logging.DEBUG)

info = logger.info
debug = logger.debug
out = logger.warning