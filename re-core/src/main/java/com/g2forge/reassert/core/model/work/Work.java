package com.g2forge.reassert.core.model.work;

import com.g2forge.alexandria.java.adt.name.IStringNamed;
import com.g2forge.reassert.core.model.IVertex;

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
public class Work implements IVertex, IStringNamed {
	protected final String name;

	protected final IWorkType type;

	@Override
	public boolean isMaterial() {
		return false;
	}
}