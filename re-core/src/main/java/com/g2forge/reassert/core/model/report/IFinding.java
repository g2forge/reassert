package com.g2forge.reassert.core.model.report;

import org.slf4j.event.Level;

public interface IFinding {
	public IFinding getInnermostFinding();

	public Level getLevel();
}
