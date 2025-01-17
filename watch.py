from __future__ import annotations

import logging
import os
import re
import shutil
import signal
from itertools import chain
from pathlib import Path
from threading import Event

from typing_extensions import assert_never
from watchfiles import Change, DefaultFilter, watch

ROOT: Path = Path(__file__).parent

class ElispFiles(DefaultFilter):
    def __call__(self, change: Change, path: str) -> bool:
        return (
            super().__call__(change, path) and
            re.match(r'[a-z-]+\.el', path) is not None
        )

def get_logging_env() -> int:
    """
    Get default logging level from environment WATCH_LEVEL.
    """
    DEFAULT = logging.INFO
    level = os.getenv("WATCH_LEVEL", DEFAULT)
    if isinstance(level, int):
        return level

    LEVELS: dict[str, int] = {
        'debug': logging.DEBUG,
        'info': logging.INFO,
        'warning': logging.WARNING,
        'error': logging.ERROR,
        'critical': logging.CRITICAL,
    }
    return LEVELS.get(level, DEFAULT)

def copy_file(src: Path) -> None:
    dst = ROOT / src.name

    match src.parts:
        case [*_comp, "packages", file]:
            dst = ROOT / "packages" / file

    logging.debug("Copy '%s' to '%s'", src, dst)
    try:
        shutil.copy2(src, dst)
    except (shutil.SameFileError | OSError) as exc:
        logging.error("Error copying '%s': %s", src.name, exc, exc_info=exc)
        signal.raise_signal(signal.SIGINT)
    else:
        logging.info("Copied '%s'", src)

def on_change(change: Change, path: Path) -> None:
    """
    Called for every file change.
    """
    match change:
        case Change.added:
            logging.debug("'%s' was added", path)
            copy_file(path)

        case Change.modified:
            logging.debug("'%s' was modified", path)
            copy_file(path)

        case Change.deleted:
            logging.debug("'%s' was deleted", path)

        case _:
            assert_never(change)

def on_keyboard_interrupt(event: Event) -> None:
    logging.info("Keyboard interrupt detected. Ending file watches.")
    event.set()

def main():
    level = get_logging_env()
    logging.basicConfig(level=level)
    logging.info("Set logging level to '%s'", logging.getLevelName(level))
    event = Event()
    it = chain(
        Path("~/.emacs.d/extensions").expanduser().glob("*.el"),
        Path("~/.emacs.d/extensions/packages").expanduser().glob("*.el")
    )
    signal.signal(signal.SIGINT, lambda x, y: on_keyboard_interrupt(event))
    for changes in watch(*it, recursive=False, stop_event=event):
        for change, path in changes:
            on_change(change, Path(path))

if __name__ == '__main__':
    main()
