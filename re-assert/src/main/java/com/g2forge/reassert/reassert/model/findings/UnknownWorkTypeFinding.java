package com.g2forge.reassert.reassert.model.findings;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.report.IFinding;

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
public class UnknownWorkTypeFinding implements IFinding {
	protected final Throwable throwable;

	@Override
	public Level getLevel() {
		return Level.ERROR;
	}
}
