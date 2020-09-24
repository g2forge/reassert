package com.g2forge.reassert.contract.algorithm.work.model.finding;

import org.slf4j.event.Level;

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
public class UnknownWorkFinding implements IWorkFinding {
	protected final Throwable throwable;

	@Override
	public Level getLevel() {
		return Level.ERROR;
	}
}
