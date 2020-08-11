package com.g2forge.reassert.reassert.test.finding;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.report.ITerminalFinding;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TestFinding implements ITerminalFinding {
	protected final Level level;

	protected final String message;
}
