package com.g2forge.reassert.contract.algorithm.work.model.rule;

import com.g2forge.reassert.core.model.work.IWorkType;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class RuleWorkType implements IWorkType {
	protected final IWorkRule rule;
}
