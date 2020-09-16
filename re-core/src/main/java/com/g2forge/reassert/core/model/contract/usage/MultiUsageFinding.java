package com.g2forge.reassert.core.model.contract.usage;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.report.ITerminalFinding;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class MultiUsageFinding implements ITerminalFinding {
	@Override
	public Level getLevel() {
		return Level.ERROR;
	}
}
