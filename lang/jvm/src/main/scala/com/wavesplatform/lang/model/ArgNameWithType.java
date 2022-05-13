package com.wavesplatform.lang.model;

public class ArgNameWithType {
    private final String name;
    private final String type;

    public ArgNameWithType(String name, String type) {
        this.name = name;
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }
}
